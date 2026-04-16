"""基础统计学算法（零第三方依赖版）

本模块仅使用 Python 标准库，适合受限环境快速运行。
包含：
1) 描述性统计（均值、方差、标准差、分位数、IQR）
2) 常见分布模拟（正态、均匀、指数）
3) 相关系数（Pearson）
4) 单样本 t 检验（双侧，提供 t 统计量 + 正态近似 p 值）
5) 两独立样本 Welch t 检验（双侧，提供 t 统计量 + 近似 p 值）
6) 单因素方差分析（One-way ANOVA，提供 F 统计量 + 近似 p 值）

说明：由于不依赖 scipy，这里的 p 值使用大样本正态近似，教学演示足够，
若用于严格科研/生产，请切换 scipy.stats 做精确分布计算。
"""

from __future__ import annotations

from dataclasses import asdict, dataclass
from math import erfc, exp, sqrt
from random import Random
from typing import Dict, Iterable, List, Sequence


ArrayLike = Iterable[float]


@dataclass
class DescriptiveStats:
    count: int
    mean: float
    median: float
    variance: float
    std_dev: float
    min_value: float
    max_value: float
    q1: float
    q3: float
    iqr: float


def _to_list(data: ArrayLike) -> List[float]:
    values = [float(x) for x in data]
    if not values:
        raise ValueError("输入数据不能为空")
    return values


def _mean(values: Sequence[float]) -> float:
    return sum(values) / len(values)


def _variance(values: Sequence[float], sample: bool = True) -> float:
    n = len(values)
    if n == 1:
        return 0.0
    mu = _mean(values)
    ss = sum((x - mu) ** 2 for x in values)
    denom = n - 1 if sample else n
    return ss / denom


def _percentile(sorted_values: Sequence[float], p: float) -> float:
    """线性插值分位数，p in [0, 1]。"""
    n = len(sorted_values)
    if n == 1:
        return sorted_values[0]
    idx = (n - 1) * p
    low = int(idx)
    high = min(low + 1, n - 1)
    w = idx - low
    return sorted_values[low] * (1 - w) + sorted_values[high] * w


def descriptive_statistics(data: ArrayLike, sample: bool = True) -> DescriptiveStats:
    values = _to_list(data)
    ordered = sorted(values)
    variance = _variance(values, sample=sample)

    return DescriptiveStats(
        count=len(values),
        mean=_mean(values),
        median=_percentile(ordered, 0.5),
        variance=variance,
        std_dev=sqrt(variance),
        min_value=ordered[0],
        max_value=ordered[-1],
        q1=_percentile(ordered, 0.25),
        q3=_percentile(ordered, 0.75),
        iqr=_percentile(ordered, 0.75) - _percentile(ordered, 0.25),
    )


def simulate_distributions(
    n: int = 1000,
    normal_mean: float = 50,
    normal_std: float = 10,
    uniform_low: float = 20,
    uniform_high: float = 80,
    exponential_scale: float = 20,
    seed: int = 123,
) -> Dict[str, List[float]]:
    rng = Random(seed)
    return {
        "normal": [rng.gauss(normal_mean, normal_std) for _ in range(n)],
        "uniform": [rng.uniform(uniform_low, uniform_high) for _ in range(n)],
        "exponential": [rng.expovariate(1 / exponential_scale) for _ in range(n)],
    }


def pearson_correlation(x: ArrayLike, y: ArrayLike) -> float:
    xs = _to_list(x)
    ys = _to_list(y)
    if len(xs) != len(ys):
        raise ValueError("x 和 y 的长度必须一致")

    mx, my = _mean(xs), _mean(ys)
    num = sum((a - mx) * (b - my) for a, b in zip(xs, ys))
    den_x = sqrt(sum((a - mx) ** 2 for a in xs))
    den_y = sqrt(sum((b - my) ** 2 for b in ys))
    if den_x == 0 or den_y == 0:
        raise ValueError("输入数据方差为 0，无法计算相关系数")
    return num / (den_x * den_y)


def _normal_two_tailed_p_from_z(z: float) -> float:
    """双侧 p 值（正态近似），使用 erfc。"""
    return erfc(abs(z) / sqrt(2))


def one_sample_t_test(data: ArrayLike, mu: float = 0.0) -> Dict[str, float]:
    values = _to_list(data)
    n = len(values)
    if n < 2:
        raise ValueError("单样本 t 检验至少需要 2 个样本")

    x_bar = _mean(values)
    s = sqrt(_variance(values, sample=True))
    if s == 0:
        raise ValueError("样本标准差为 0，无法计算 t 统计量")

    t_stat = (x_bar - mu) / (s / sqrt(n))
    # 无 scipy 时采用正态近似 p 值（n 较大时更可靠）
    p_value_approx = _normal_two_tailed_p_from_z(t_stat)

    return {"t_stat": t_stat, "p_value_approx": p_value_approx, "df": float(n - 1)}


def welch_t_test(group1: ArrayLike, group2: ArrayLike) -> Dict[str, float]:
    g1 = _to_list(group1)
    g2 = _to_list(group2)
    n1, n2 = len(g1), len(g2)
    if n1 < 2 or n2 < 2:
        raise ValueError("Welch t 检验要求每组至少 2 个样本")

    m1, m2 = _mean(g1), _mean(g2)
    v1, v2 = _variance(g1, sample=True), _variance(g2, sample=True)

    se_sq = v1 / n1 + v2 / n2
    if se_sq == 0:
        raise ValueError("标准误为 0，无法计算 t 统计量")

    t_stat = (m1 - m2) / sqrt(se_sq)
    numerator = se_sq**2
    denominator = ((v1 / n1) ** 2) / (n1 - 1) + ((v2 / n2) ** 2) / (n2 - 1)
    df = numerator / denominator
    p_value_approx = _normal_two_tailed_p_from_z(t_stat)

    return {"t_stat": t_stat, "p_value_approx": p_value_approx, "df": df}


def one_way_anova(*groups: ArrayLike) -> Dict[str, float]:
    if len(groups) < 2:
        raise ValueError("ANOVA 至少需要两个组")

    arrays = [_to_list(g) for g in groups]
    if any(len(g) < 2 for g in arrays):
        raise ValueError("ANOVA 每个组至少需要 2 个样本")

    all_values = [x for group in arrays for x in group]
    grand_mean = _mean(all_values)

    ss_between = sum(len(g) * (_mean(g) - grand_mean) ** 2 for g in arrays)
    ss_within = sum(sum((x - _mean(g)) ** 2 for x in g) for g in arrays)

    k = len(arrays)
    n = len(all_values)
    df_between = k - 1
    df_within = n - k

    ms_between = ss_between / df_between
    ms_within = ss_within / df_within
    if ms_within == 0:
        raise ValueError("组内方差为 0，无法计算 F 统计量")

    f_stat = ms_between / ms_within

    # 简化近似：将 F 统计量转换为 z 分数用于演示（严格场景请使用 scipy）
    z_approx = sqrt(max(f_stat, 0))
    p_value_approx = _normal_two_tailed_p_from_z(z_approx)

    return {
        "f_stat": f_stat,
        "p_value_approx": p_value_approx,
        "df_between": float(df_between),
        "df_within": float(df_within),
    }


def demo() -> None:
    distributions = simulate_distributions(n=200, seed=42)

    print("=== 描述性统计（normal）===")
    print(asdict(descriptive_statistics(distributions["normal"])))

    print("\n=== 相关系数示例 ===")
    x = distributions["normal"]
    noise_rng = Random(42)
    y = [0.6 * xi + noise_rng.gauss(0, 5) for xi in x]
    print({"pearson_r": pearson_correlation(x, y)})

    print("\n=== 单样本 t 检验 ===")
    print(one_sample_t_test(distributions["normal"], mu=50))

    print("\n=== Welch t 检验 ===")
    rng7 = Random(7)
    rng8 = Random(8)
    g1 = [rng7.gauss(70, 12) for _ in range(120)]
    g2 = [rng8.gauss(66, 14) for _ in range(110)]
    print(welch_t_test(g1, g2))

    print("\n=== 单因素方差分析 ===")
    rng9 = Random(9)
    g3 = [rng9.gauss(72, 10) for _ in range(100)]
    print(one_way_anova(g1, g2, g3))


if __name__ == "__main__":
    demo()
