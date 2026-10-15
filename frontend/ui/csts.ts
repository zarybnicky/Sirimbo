import type { CompetitionType } from '@/graphql';
import type { AgeGroupRulesQuery } from '@/graphql/AgeGroups';
import type {
  CompetitionCategoryFragment,
  FederationCategoryProgressFragment,
} from '@/graphql/Federation';

export const ageGroupNames: Record<string, string> = {
  Under_8: 'Do 8 let',
  Juvenile: 'Děti',
  Juvenile_I: 'Děti I',
  Juvenile_II: 'Děti II',
  Junior: 'Junioři',
  Junior_I: 'Junioři I',
  Junior_II: 'Junioři II',
  Youth: 'Mládež',
  Under_21: 'Do 21 let',
  Adult: 'Dospělí',
  Senior: 'Senioři',
  Senior_I: 'Senioři I',
  Senior_Ia: 'Senioři Ia',
  Senior_Ib: 'Senioři Ib',
  Senior_II: 'Senioři II',
  Senior_IIa: 'Senioři IIa',
  Senior_IIb: 'Senioři IIb',
  Senior_III: 'Senioři III',
  Senior_IIIa: 'Senioři IIIa',
  Senior_IIIb: 'Senioři IIIb',
  Senior_IV: 'Senioři IV',
  Senior_IVa: 'Senioři IVa',
  Senior_IVb: 'Senioři IVb',
  Senior_V: 'Senioři V',
};

export type AgeRange = {
  start: { value: number } | null;
  end: { value: number } | null;
};

export function compareAgeRanges(a: AgeRange | null, b: AgeRange | null) {
  if (!a || !b) return Number(!a) - Number(!b);
  return (
    (a.start?.value ?? -Infinity) - (b.start?.value ?? -Infinity) ||
    (a.end?.value ?? Infinity) - (b.end?.value ?? Infinity) ||
    0
  );
}

export function formatAgeGroup(code: string | null | undefined, name?: string | null) {
  return code ? (ageGroupNames[code] ?? name ?? code) : undefined;
}

export function getCoupleAgeGroups(
  first: string | null | undefined,
  second: string | null | undefined,
  groups: AgeGroupRulesQuery['ageGroupsList'] | undefined,
) {
  if (!first || !second) return [];
  const year = new Date().getFullYear();
  const ages = [year - Number(first.slice(0, 4)), year - Number(second.slice(0, 4))];
  const older = Math.max(...ages);
  const younger = Math.min(...ages);
  return (groups ?? []).filter((group) =>
    group.coupleRules.some(
      (rule) =>
        older >= (rule.olderAge.start?.value ?? -Infinity) &&
        older < (rule.olderAge.end?.value ?? Infinity) &&
        younger >= (rule.youngerAge.start?.value ?? -Infinity) &&
        younger < (rule.youngerAge.end?.value ?? Infinity),
    ),
  );
}

const hiddenCategoryClasses = new Set(['Novice', 'Bronze', 'Silver', 'Gold', 'Entry']);
const classOrder = ['E', 'D', 'C', 'B', 'A', 'M'];

export function formatCstsClass(value: string | null | undefined) {
  return !value || value === 'Open' ? null : value === 'S' ? 'M' : value;
}

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
    formatAgeGroup(category.ageGroup, category.ageGroupDetails?.name),
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

export function getBestCstsProgress(
  progressList: readonly FederationCategoryProgressFragment[] | null | undefined,
  discipline: string,
) {
  return (progressList ?? [])
    .filter((progress) => progress.competitor?.federation === 'csts')
    .filter((progress) => !hiddenCategoryClasses.has(progress?.category?.class ?? ''))
    .filter((progress) => progress?.category?.discipline === discipline)
    .reduce<FederationCategoryProgressFragment | null>((best, progress) => {
      const bestRank = best
        ? classOrder.indexOf(formatCstsClass(best?.category?.class) ?? '')
        : -1;
      const progressRank = classOrder.indexOf(
        formatCstsClass(progress?.category?.class) ?? '',
      );

      return progressRank > bestRank ? progress : best;
    }, null);
}
