import type { CstsProgressRecordFragment } from '@/graphql/Cohorts';

const hiddenCategoryClasses = new Set(['Novice', 'Bronze', 'Silver', 'Gold', 'Entry']);
const classOrder = ['E', 'D', 'C', 'B', 'A', 'M'];

export function formatCstsClass(value: string | null | undefined) {
  return !value ? null : value === 'S' ? 'M' : value;
}

export function formatCstsCategoryName(
  category:
    | {
        ageGroup?: string | null;
        class?: string | null;
        discipline?: string | null;
        series?: string | null;
      }
    | null
    | undefined,
) {
  if (!category) return '';
  const { ageGroup, discipline, series } = category;

  return [
    series === 'DanceSport' ? undefined : series === 'DanceForAll' ? 'TPV' : '',
    ageGroup?.toLowerCase() === 'adult'
      ? 'Dospělí'
      : category.ageGroup?.replace('_', ' '),
    formatCstsClass(category.class),
    discipline?.toLowerCase() === 'standard'
      ? 'STT'
      : discipline?.toLowerCase() === 'latin'
        ? 'LAT'
        : discipline,
  ]
    .filter(Boolean)
    .join(' ');
}

export function getBestCstsProgress<T extends CstsProgressRecordFragment>(
  progressList: readonly T[] | null | undefined,
  discipline: string,
) {
  return (progressList ?? [])
    .filter((progress) => !hiddenCategoryClasses.has(progress?.category?.class ?? ''))
    .filter((progress) => progress?.category?.discipline === discipline)
    .reduce<CstsProgressRecordFragment | null>((best, progress) => {
      const bestRank = best
        ? classOrder.indexOf(formatCstsClass(best?.category?.class) ?? '')
        : -1;
      const progressRank = classOrder.indexOf(
        formatCstsClass(progress?.category?.class) ?? '',
      );

      return progressRank > bestRank ? progress : best;
    }, null);
}
