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
      }
    | null
    | undefined,
) {
  if (!category) return '';
  const ageGroup =
    category.ageGroup?.toLowerCase() === 'adult' ? 'Dospělí' : category.ageGroup;
  const disciplineKey = category.discipline?.toLowerCase();
  const discipline =
    disciplineKey === 'standard'
      ? 'STT'
      : disciplineKey === 'latin'
        ? 'LAT'
        : disciplineKey === 'dancesport'
          ? null
          : category.discipline;

  return [ageGroup, formatCstsClass(category.class), discipline]
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
