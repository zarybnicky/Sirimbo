import type { CompetitionType } from '@/graphql';
import type { CstsProgressRecordFragment } from '@/graphql/Cohorts';
import type { CompetitionCategoryFragment } from '@/graphql/Federation';

const hiddenCategoryClasses = new Set(['Novice', 'Bronze', 'Silver', 'Gold', 'Entry']);
const classOrder = ['E', 'D', 'C', 'B', 'A', 'M'];

export function formatCstsClass(value: string | null | undefined) {
  return !value || value === 'Open' ? null : value === 'S' ? 'M' : value;
}

const ageGroups = {
  ADULT: 'Dospělí',
  YOUTH: 'Mládež',
  JUVENILE_II: 'Děti 2',
  JUVENILE_I: 'Děti 1',
  JUVENILE: 'Děti',
} as Record<string, string>;

const typePrefix: Partial<Record<CompetitionType, string>> = {
  LEAGUE: 'TL',
  CHAMPIONSHIP: 'MČR',
  SUPER_LEAGUE: 'STL',
};

const typeSuffix: Partial<Record<CompetitionType, string>> = {
  CUP: 'Cup',
  G_CUP: 'G-Cup',
  TOP_LEVEL: 'TopLevel',
};

export function formatCstsCategoryName(
  category: CompetitionCategoryFragment | null | undefined,
  competitionType: CompetitionType | null | undefined,
) {
  if (!category) return '';
  const { discipline, series, competitorType } = category;

  return [
    competitionType && typePrefix[competitionType],
    series === 'DanceSport' ? undefined : series === 'DanceForAll' ? 'TPV' : series,
    ageGroups[category.ageGroup.toUpperCase()] || category.ageGroup?.replace('_', ' '),
    formatCstsClass(category.class),
    discipline === 'Standard_Latin'
      ? 'STT+LAT'
      : discipline === 'TenDance'
        ? '10T'
        : discipline?.toLowerCase() === 'standard' && competitorType === 'SOLO'
          ? 'SoSTT'
          : discipline?.toLowerCase() === 'latin' && competitorType === 'SOLO'
            ? 'SoLAT'
            : discipline?.toLowerCase() === 'standard'
              ? 'STT'
              : discipline?.toLowerCase() === 'latin'
                ? 'LAT'
                : discipline,
    competitionType && typeSuffix[competitionType],
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
