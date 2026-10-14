import type { CompetitionType } from '@/graphql';
import type {
  CompetitionCategoryFragment,
  FederationCategoryProgressFragment,
} from '@/graphql/Federation';

export type AgeConstraint = { maxAge?: number; minAge?: number };

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

type AgeRange = {
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

export const cstsAgeCategories: {
  id: string;
  older: AgeConstraint;
  younger: AgeConstraint;
}[] = [
  { id: 'Under_8', older: { maxAge: 8 }, younger: { maxAge: 8 } },
  { id: 'Juvenile_I', older: { maxAge: 10 }, younger: { maxAge: 10 } },
  {
    id: 'Juvenile_II',
    older: { maxAge: 12, minAge: 10 },
    younger: { maxAge: 12 },
  },
  {
    id: 'Junior_I',
    older: { maxAge: 14, minAge: 12 },
    younger: { maxAge: 14 },
  },
  {
    id: 'Junior_II',
    older: { maxAge: 16, minAge: 14 },
    younger: { maxAge: 16 },
  },
  {
    id: 'Youth',
    older: { maxAge: 19, minAge: 16 },
    younger: { maxAge: 19 },
  },
  {
    id: 'Under_21',
    older: { maxAge: 21, minAge: 16 },
    younger: { maxAge: 21 },
  },
  { id: 'Adult', older: { minAge: 19 }, younger: { maxAge: 35 } },
  { id: 'Senior_I', older: { minAge: 35 }, younger: { minAge: 30 } },
  { id: 'Senior_II', older: { minAge: 45 }, younger: { minAge: 40 } },
  {
    id: 'Senior_III',
    older: { minAge: 55 },
    younger: { minAge: 50 },
  },
  { id: 'Senior_IV', older: { minAge: 65 }, younger: { minAge: 60 } },
];

export function getCstsAgeCategory(
  birthDate: string | null | undefined,
  partnerBirthDate: string | null | undefined = birthDate,
) {
  if (!birthDate || !partnerBirthDate) return;
  const year = new Date().getFullYear();
  const ages = [birthDate, partnerBirthDate].map(
    (date) => year - Number(date.slice(0, 4)),
  );
  const older = Math.max(...ages);
  const younger = Math.min(...ages);
  const categories = cstsAgeCategories.filter(
    (category) =>
      category.id !== 'Under_21' &&
      older >= (category.older.minAge ?? 0) &&
      older < (category.older.maxAge ?? Infinity) &&
      younger >= (category.younger.minAge ?? 0) &&
      younger < (category.younger.maxAge ?? Infinity),
  );
  return older < 19 ? categories[0] : categories.at(-1);
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
    (category.ageGroup && ageGroupNames[category.ageGroup]) ??
      category.ageGroupDetails?.name ??
      category.ageGroup,
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
