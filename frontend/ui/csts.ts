import type { CompetitionType } from '@/graphql';
import type { CstsProgressRecordFragment } from '@/graphql/Cohorts';
import type { CompetitionCategoryFragment } from '@/graphql/Federation';

const hiddenCategoryClasses = new Set(['Novice', 'Bronze', 'Silver', 'Gold', 'Entry']);
const classOrder = ['E', 'D', 'C', 'B', 'A', 'M'];

export function formatCstsClass(value: string | null | undefined) {
  return !value || value === 'Open' ? null : value === 'S' ? 'M' : value;
}

export function formatCstsCategoryName(category?: CompetitionCategoryFragment | null) {
  if (!category) return '';
  const { ageGroup, discipline, series } = category;

  return [
    series === 'DanceSport' ? undefined : series === 'DanceForAll' ? 'TPV' : series,
    ageGroup?.toLowerCase() === 'adult'
      ? 'Dospělí'
      : ageGroup?.toLowerCase() === 'youth'
        ? 'Mládež'
        : ageGroup?.toLowerCase() === 'juvenile_ii'
          ? 'Děti 2'
          : ageGroup?.toLowerCase() === 'juvenile_i'
            ? 'Děti 1'
            : category.ageGroup?.replace('_', ' '),
    formatCstsClass(category.class),
    discipline === 'Standard_Latin'
      ? 'STT+LAT'
      : discipline?.toLowerCase() === 'standard'
        ? 'STT'
        : discipline?.toLowerCase() === 'latin'
          ? 'LAT'
          : discipline,
  ]
    .filter(Boolean)
    .join(' ');
}

export function formatCstsCompetitionType(value: CompetitionType | null | undefined) {
  switch (value) {
    case 'CUP':
      return 'Cup';
    case 'LEAGUE':
      return 'TL';
    case 'CHAMPIONSHIP':
      return 'MČR';
    case 'TOP_LEVEL':
      return 'TopLevel';
    case 'SUPER_LEAGUE':
      return 'STL';
    case 'G_CUP':
      return 'G-Cup';
    default:
    case 'RANKING':
      return null;
  }
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
