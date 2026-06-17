import type { CompetitionType } from '@/graphql';
import type { CstsProgressRecordFragment } from '@/graphql/Cohorts';
import type { CompetitionCategoryFragment } from '@/graphql/Federation';

const hiddenCategoryClasses = new Set(['Novice', 'Bronze', 'Silver', 'Gold', 'Entry']);
const classOrder = ['E', 'D', 'C', 'B', 'A', 'M'];
const CSTS_BASE_URL = 'https://www.csts.cz/dancesport';
const numericIdPattern = /^\d+$/;

export const CSTS_COMPETITION_CALENDAR_URL = `${CSTS_BASE_URL}/kalendar_akci`;

function cstsPathPart(value: number | string | null | undefined, pattern?: RegExp) {
  if (value === null || value === undefined || value === '') return null;
  if (typeof value === 'number' && !Number.isFinite(value)) return null;
  const text = String(value).trim();
  return text && (!pattern || pattern.test(text)) ? encodeURIComponent(text) : null;
}

export function cstsPersonUrl(idt: number | string | null | undefined) {
  const id = cstsPathPart(idt, numericIdPattern);
  return id ? `${CSTS_BASE_URL}/evidence/lide/${id}/osobni_udaje` : null;
}

export function cstsCompetitionResultUrl(
  eventId: number | string | null | undefined,
  competitionId: number | string | null | undefined,
) {
  const event = cstsPathPart(eventId);
  const competition = cstsPathPart(competitionId);
  return event && competition
    ? `${CSTS_BASE_URL}/vysledky_soutezi/event/${event}/competition/${competition}`
    : null;
}

export function formatCstsClass(value: string | null | undefined) {
  return !value || value === 'Open' ? null : value === 'S' ? 'M' : value;
}

function formatCstsAgeGroup(ageGroup: string) {
  return ageGroup?.toLowerCase() === 'adult'
    ? 'Dospělí'
    : ageGroup?.toLowerCase() === 'youth'
      ? 'Mládež'
      : ageGroup?.toLowerCase() === 'juvenile_ii'
        ? 'Děti 2'
        : ageGroup?.toLowerCase() === 'juvenile_i'
          ? 'Děti 1'
          : ageGroup?.replace('_', ' ');
}

export function formatCstsCategoryName(
  category: CompetitionCategoryFragment | null | undefined,
  competitionType: CompetitionType | null | undefined,
) {
  if (!category) return '';
  const { discipline, series, competitorType } = category;

  return [
    formatCstsCompetitionType(competitionType),
    series === 'DanceSport' ? undefined : series === 'DanceForAll' ? 'TPV' : series,
    formatCstsAgeGroup(category.ageGroup),
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
  ]
    .filter(Boolean)
    .join(' ');
}

function formatCstsCompetitionType(value: CompetitionType | null | undefined) {
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
