type CstsCategoryLike = {
  ageGroup?: string | null;
  class?: string | null;
  discipline?: string | null;
} | null;

type CstsProgressLike = {
  category?: CstsCategoryLike;
} | null;

const hiddenCategoryClasses = new Set(['Novice', 'Bronze', 'Silver', 'Gold', 'Entry']);
const classOrder = ['E', 'D', 'C', 'B', 'A', 'M'];

export function normalizeCstsClass(value: string | null | undefined) {
  if (!value) return null;
  return value === 'S' ? 'M' : value;
}

export function getBestCstsProgress<T extends CstsProgressLike>(
  progressList: readonly T[] | null | undefined,
  discipline?: string,
) {
  return (progressList ?? [])
    .filter(
      (
        progress,
      ): progress is T & {
        category: NonNullable<NonNullable<T>['category']>;
      } => progress?.category != null,
    )
    .filter((progress) => !hiddenCategoryClasses.has(progress.category.class ?? ''))
    .filter((progress) =>
      discipline ? progress.category.discipline === discipline : true,
    )
    .reduce<
      | (T & {
          category: NonNullable<NonNullable<T>['category']>;
        })
      | null
    >((best, progress) => {
      const bestRank = best
        ? classOrder.indexOf(normalizeCstsClass(best.category.class) ?? '')
        : -1;
      const progressRank = classOrder.indexOf(
        normalizeCstsClass(progress.category.class) ?? '',
      );

      return progressRank > bestRank ? progress : best;
    }, null);
}
