import type { CompetitionType } from '@/graphql';
import type { AgeGroupRulesQuery } from '@/graphql/AgeGroups';
import type {
  CompetitionCategoryLabelFragment,
  FederationCategoryProgressFragment,
} from '@/graphql/Federation';
import { compareByOrder, compareNaturalText } from '@/lib/compare';

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

type AgeGroup = {
  age: AgeRange | null;
  coupleRules: readonly { olderAge: AgeRange }[];
};

type Category = {
  ageGroupDetails: AgeGroup | null;
  class: string;
  discipline: string;
};

export function compareAgeRanges(
  a: AgeRange | null | undefined,
  b: AgeRange | null | undefined,
) {
  if (!a || !b) return Number(!a) - Number(!b);
  return (
    (a.start?.value ?? -Infinity) - (b.start?.value ?? -Infinity) ||
    (a.end?.value ?? Infinity) - (b.end?.value ?? Infinity) ||
    0
  );
}

export function compareAgeGroups(
  a: AgeGroup | null | undefined,
  b: AgeGroup | null | undefined,
) {
  return compareAgeRanges(
    a?.coupleRules[0]?.olderAge ?? a?.age,
    b?.coupleRules[0]?.olderAge ?? b?.age,
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

const hiddenClasses = ['Entry', 'Novice', 'Bronze', 'Silver', 'Gold'];
const hiddenCategoryClasses = new Set(hiddenClasses);
const classOrder = ['E', 'D', 'C', 'B', 'A', 'S', 'M'];

export const compareClasses = compareByOrder(
  [...hiddenClasses, ...classOrder, 'Open'].toReversed(),
);

const compareDisciplineOrder = compareByOrder(['STANDARD', 'LATIN', 'TENDANCE']);

export function compareDisciplines(a: string, b: string) {
  return compareDisciplineOrder(
    a.toUpperCase().replaceAll(' ', ''),
    b.toUpperCase().replaceAll(' ', ''),
  );
}

export function compareCategories(a: Category | null, b: Category | null) {
  if (!a || !b) return Number(!a) - Number(!b);
  return (
    compareAgeGroups(a.ageGroupDetails, b.ageGroupDetails) ||
    compareClasses(a.class, b.class) ||
    compareNaturalText(a.class, b.class) ||
    compareDisciplines(a.discipline, b.discipline) ||
    compareNaturalText(a.discipline, b.discipline)
  );
}

export function formatClass(value: string | null | undefined) {
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

export function formatCategoryName(
  federation: string | null | undefined,
  category: CompetitionCategoryLabelFragment | null | undefined,
  competitionType?: CompetitionType | null,
) {
  if (!category) return '';
  if (federation === 'wdsf') return formatWdsfCategoryName(category);
  return federation === 'csts'
    ? formatCstsCategoryName(category, competitionType)
    : (category?.name ?? '');
}

const wdsfAcronyms = new Set(['PD', 'NS', 'DSE', 'GP']);

const wdsfClassNames: Record<string, string> = {
  'GRAND SLAM': 'GrandSlam',
  'WORLD OPEN NS': 'World Open (new series)',
  'WORLD CHAL CUP': 'World Challenge Cup',
  'WORLD CUP OTHER': 'World Cup',
  'DS FESTIVAL CH': 'DanceSport Festival Championship',
  'PD TNS': 'PD Teacher & Student',
  'ASIA CUP': 'Asian Cup',
  'ASIA CHAMPIONSHIP': 'Asian Championship',
  'ASIA PACIFIC CHAMPIONSHIP': 'Asian Pacific Championship',
  'SEA CHAMPIONSHIP': 'South East Asian Championship',
  'SEA GAMES': 'South East Asian Games',
  'BALK CHAMPIONSHIP': 'Balkan Championship',
  'SUB CHAMPIONSHIP': 'Sub Continental Championship',
  'NORTH AMERICA CHAMPIONSHIP': 'North American Championship',
  'EUROPE CUP': 'European Cup',
  'EUROPE CHAMPIONSHIP': 'European Championship',
  'EUROPE OPEN CHAMPIONSHIP': 'Open European Championship',
  'EUROPE NORTH CHAMPIONSHIP': 'North European Championship',
  'EUROPE EAST CHAMPIONSHIP': 'East European Championship',
  'EUROPE CENTRAL CHAMPIONSHIP': 'Central European Championship',
  'SOUTH EUROPEAN CHAMPIONSHIP': 'South European Championship',
  'EU CHAMPIONSHIP': 'European Union Championship',
  'EUROPE UNI': 'European University',
  'EUROPE MASTER': 'European Master Games',
  'DSE NT CH': 'DSE National Teams Championship',
  'DSE U CHAMPIONSHIP': 'DSE Universities Championship',
  'DSE CHILDREN GP': 'DSE Children Grand Prix',
  'DSE CHILDREN T CH': 'DSE Children Teams Championship',
  'INT TEAM MATCH': 'International Team Match',
  'ST OPEN': 'Open',
  'ST WORLD CHAMPIONSHIP': 'World Championship',
  'ST EUROPE CHAMPIONSHIP': 'European Championship',
  'DS OPEN': 'Open',
  'DS WORLD CHAMPIONSHIP': 'World Championship',
  'DS EUROPE CHAMPIONSHIP': 'European Championship',
  'HIPHOP WORLD CHAMPIONSHIP': 'World Championship',
  'HIPHOP EURO CHAMPIONSHIP': 'European Championship',
  'SMOOTH OPEN': 'Open',
  'SMOOTH WORLD CHAMPIONSHIP': 'World Championship',
  'OPEN CARRIBEAN': 'Open',
  'WORLD CHAMPIONSHIP CAR': 'World Championship',
  'URBAN CHAL': 'BfG Challenge Series',
  'URBAN WORLD SER': 'BfG World Series',
  'URBAN INT SER': 'BfG International Series',
  'URBAN CONT': 'Continental Qualifier',
  'URBAN CONT CH': 'Continental Championship',
  'URBAN CONT GAMES': 'Continental Games',
  'URBAN WCH': 'World Championships',
  'URBAN OPEN': 'Open',
  'URBAN YOG': 'Youth Olympic Games',
  'URBAN WUG': 'World Urban Games',
  'URBAN OQS': 'Olympic Qualifier Series',
  'IWGA WG': 'IWGA The World Games',
  'OLYMP': 'Olympic Games',
  'GAMES QUALIFIER': 'Sport Series Games Qualifier',
  'WDSG': 'World DanceSport Games',
};

const wdsfDisciplineNames: Record<string, string> = {
  'SYN LATIN': 'Synchro Latin',
  'SYN STANDARD': 'Synchro Standard',
  'CHOR LATIN': 'Choreogr. Latin',
  'CHOR STANDARD': 'Choreogr. Standard',
  '3D LATIN': '3 Dance',
  '3D STANDARD': '3 Dance',
  'SALSA ONE': 'Salsa On One',
  'SALSA TWO': 'Salsa On Two',
  'BREAK': 'Breaking',
  'HIPHOP': 'Hip-Hop Freestyle',
  'HH CHOREO': 'Hip-Hop Choreography',
  'DISCO': 'Disco Dance',
  'ACRO DISCO': 'Acrobatic Disco Dance',
  'MODERN PERFOMING DANCE': 'Modern & Contemporary Dance',
  'JAZZ': 'Jazz Dance',
  'ROCK N ROLL': "Rock 'n Roll",
};

const wdsfAgeGroupNames: Record<string, string> = {
  Rising_Star: 'Rising Stars',
  Juvenile_T: 'Juvenile',
  Junior_T: 'Junior',
  Adult_T: 'Adult',
  Adult_O: 'Adult',
  Senior_T: 'Senior',
  Senior_O: 'Senior',
};

const wdsfDanceFormNames: Record<string, string> = {
  '': '',
  'mixed': '',
  'M': 'Male',
  'F': 'Female',
  '1 M': 'Male',
  '1 F': 'Female',
  'DUO M': 'Duo Male',
  'DUO F': 'Duo Female',
  'DUO MIX': 'Duo Mixed',
  'TRIO X': 'Trio',
  'TRIO F': 'Trio Female',
  '1 VS 1': '1 vs 1',
  '2 VS 2': 'Team 2 vs 2',
  '3 VS 3': 'Team 3 vs 3',
  '5 VS 5': 'Team 5 vs 5',
  '6 VS 6': 'Team 6 vs 6',
  'F VS F': '1 vs 1 B-Girls',
  'M VS M': '1 vs 1 B-Boys',
};

function titleCaseWdsfLabel(value: string) {
  return value.trim().split(/[\s_]+/).map((word) => {
    const upper = word.toUpperCase();
    return wdsfAcronyms.has(upper) || /^[IVX]+$/.test(upper)
      ? upper
      : upper.charAt(0) + word.slice(1).toLowerCase();
  }).join(' ');
}

function formatWdsfCategoryName(category: CompetitionCategoryLabelFragment) {
  const { ageGroup, genderGroup } = category;
  if (
    !ageGroup ||
    !category.class.trim() ||
    !category.discipline.trim() ||
    category.class.toUpperCase() === 'CUSTOM' ||
    category.discipline.toUpperCase().startsWith('CUSTOM')
  ) {
    return category.name;
  }

  const className = wdsfClassNames[category.class] ?? titleCaseWdsfLabel(category.class);
  const disciplineCode = category.discipline.trim().replace(/^PD\s+/i, '');
  const discipline = wdsfDisciplineNames[disciplineCode] ?? titleCaseWdsfLabel(disciplineCode);
  const danceForm = wdsfDanceFormNames[genderGroup] ?? titleCaseWdsfLabel(genderGroup);

  return [
    className.startsWith('DSE ') || category.class === 'WORLD CUP OTHER' ? undefined : 'WDSF',
    className,
    discipline,
    discipline.toLowerCase().includes(danceForm.toLowerCase()) ? undefined : danceForm,
    wdsfAgeGroupNames[ageGroup] ?? titleCaseWdsfLabel(ageGroup),
  ].filter(Boolean).join(' ');
}

function formatCstsCategoryName(
  category: CompetitionCategoryLabelFragment,
  competitionType: CompetitionType | null | undefined,
) {
  const { discipline, series, competitorType } = category;
  const disciplinePrefix =
    competitorType === 'SOLO'
      ? 'So'
      : competitorType === 'DUO' || competitorType === 'TRIO'
        ? 'Sy'
        : '';

  return [
    competitionType && typePrefix[competitionType],
    series === 'DanceSport' ? undefined : series === 'DanceForAll' ? 'TPV' : series,
    formatAgeGroup(category.ageGroup, category.ageGroupDetails?.name),
    formatClass(category.class),
    discipline === 'Standard_Latin'
      ? 'STT+LAT'
      : discipline === 'TenDance'
        ? '10T'
        : discipline?.toLowerCase() === 'standard'
          ? `${disciplinePrefix}STT`
          : discipline?.toLowerCase() === 'latin'
            ? `${disciplinePrefix}LAT`
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
    .filter((x) => x.competitor?.federation === 'csts')
    .filter((x) => !hiddenCategoryClasses.has(x?.category?.class ?? ''))
    .filter((x) => x?.category?.discipline === discipline)
    .reduce<FederationCategoryProgressFragment | null>((best, x) => {
      const bestRank = best
        ? classOrder.indexOf(formatClass(best?.category?.class) ?? '')
        : -1;
      const progressRank = classOrder.indexOf(formatClass(x?.category?.class) ?? '');
      return progressRank > bestRank ? x : best;
    }, null);
}
