import { cstsMember } from './cstsMember.ts';
import type { HtmlLoader, Loader } from './types.ts';
import { cstsRanklistIndex } from './cstsRanklistIndex.ts';
import { cstsRanklist } from './cstsRanklist.ts';
import { wdsfMember } from './wdsfMember.ts';
import { wdsfMemberIndex } from './wdsfMemberIndex.ts';
import { cstsEventIndex } from './cstsEventIndex.ts';
import { cstsEventCompetitors } from './cstsEventCompetitors.ts';
import { cstsEvent } from './cstsEvent.ts';
import { cstsCompetitionResults } from './cstsCompetitionResults.ts';
import { cstsClubIndex } from './cstsClubIndex.ts';
import {
  cstsJudgeIndex,
  cstsOfficialIndex,
  cstsTrainerIndex,
} from './cstsPersonIndex.ts';
import { sztsCoupleIndex, sztsSoloIndex } from './sztsCompetitorIndex.ts';
import {
  sztsJudgeIndex,
  sztsMemberIndex,
  sztsOfficialIndex,
  sztsScrutineerIndex,
  sztsTrainerIndex,
} from './sztsPersonIndex.ts';
import { wdsfCompetitionIndex } from './wdsfCompetitionIndex.ts';
import { wdsfCompetition } from './wdsfCompetition.ts';
import { wdsfParticipantIndex } from './wdsfParticipantIndex.ts';

export const LOADERS = {
  wdsf: {
    // https://services.worlddancesport.org/api/1/person
    memberIndex: wdsfMemberIndex,

    // https://services.worlddancesport.org/api/1/person/10116109
    member: wdsfMember,

    // https://www.worlddancesport.org/Calendar/Competitions?Month=2&Year=2026 => /Events/Voesendorf-Vienna-Austria-21022026-22022026-8685
    // https://services.worlddancesport.org/api/1/competition?from=2025/11/01
    competitionIndex: wdsfCompetitionIndex,
    // https://services.worlddancesport.org/api/1/competition/64306
    competition: wdsfCompetition,

    // https://services.worlddancesport.org/api/1/participant?competitionId=64306
    participantIndex: wdsfParticipantIndex,

    // https://services.worlddancesport.org/api/1/official?competitionId=64306 - includes letters of judges
    // https://services.worlddancesport.org/api/1/couple
    // https://services.worlddancesport.org/api/1/participant?coupleId=rls-1912
    // https://services.worlddancesport.org/api/1/participant/2313241?format=json = results!
    // https://services.worlddancesport.org/api/1/ranking?ageGroup=???&discipline=???&division=???
    // https://services.worlddancesport.org/api/1/country
    // https://services.worlddancesport.org/api/1/age - includes divisions? or disciplines?
  },
  csts: {
    // Soutěžní údaje, modrý web
    // https://www.csts.cz/api/1/athletes/18038132
    member: cstsMember,

    // Ranklisty
    // https://www.csts.cz/api/1/ranklist
    // https://www.csts.cz/api/1/ranklist/6733
    ranklistIndex: cstsRanklistIndex,
    ranklist: cstsRanklist,

    // Osobní údaje, žlutý JS
    // https://www.csts.cz/api/evidence/clenove/detail-clena/osobni-udaje/18038132

    // Partneři, modrý web
    // https://www.csts.cz/api/1/competitors/couple_or_duo_registrations?filter=idt=18038132

    // Dlouhodobé hostování
    // https://www.csts.cz/api/1/loans?filter=memberIdt=18038132

    // Přestupy
    // https://www.csts.cz/api/evidence/clenove/prestupy/seznam-clena/14384?%24count=true&%24skip=0&%24top=20&%24orderby=DatPorizeni%20desc
    // https://www.csts.cz/api/evidence/clenove/prestupy/prestup/1159

    // Soutěže
    // https://www.csts.cz/api/1/events?filter=date%3E%3D2025-12-01%20AND%20date%3C%3D2026-03-01
    eventIndex: cstsEventIndex,
    event: cstsEvent,
    // https://www.csts.cz/api/1/events/327/competitions?filter=eventCompetitionId%3D327&order=type
    // eventCompetitions: cstsEventCompetitions PART of competition_events??
    // https://www.csts.cz/api/1/events/327/officials
    // eventOfficials: cstsEventOfficials PART of competition_events??
    // https://www.csts.cz/api/1/events/327/competitors
    eventCompetitors: cstsEventCompetitors,

    // Výsledky
    // https://www.csts.cz/api/1/competition_events?from=2025-11-01&to=2025-11-30
    // https://www.csts.cz/api/1/competition_events/1262
    // TODO: ^^ (propozice!)
    // https://www.csts.cz/api/1/competitions/32329/result
    competitionResults: cstsCompetitionResults,

    clubIndex: cstsClubIndex,

    divisionIndex: {
      mode: 'text',
      revalidatePeriod: '1 day',
      buildRequest: () => ({ url: new URL('https://www.csts.cz/cs/Divize') }),
      async load() {},
    } satisfies HtmlLoader,

    trainerIndex: cstsTrainerIndex,

    judgeIndex: cstsJudgeIndex,

    officialIndex: cstsOfficialIndex,
  },
  szts: {
    // soutěže https://szts.ksis.eu/menu.php?akcia=KS
    competitionIndex: {
      mode: 'text',
      revalidatePeriod: '1 day',
      buildRequest: () => ({ url: new URL('https://szts.ksis.eu/menu.php?akcia=KS') }),
      async load() {},
    } satisfies HtmlLoader,

    // clenove https://szts.ksis.eu/menu.php?akcia=CZ
    memberIndex: sztsMemberIndex,

    // trenéři https://szts.ksis.eu/menu.php?akcia=CZT
    trainerIndex: sztsTrainerIndex,

    // funkcionáři https://szts.ksis.eu/menu.php?akcia=CZF
    officialIndex: sztsOfficialIndex,

    // sčitatelé https://szts.ksis.eu/menu.php?akcia=CZS
    scrutineerIndex: sztsScrutineerIndex,

    // porotci https://szts.ksis.eu/menu.php?akcia=CZR
    judgeIndex: sztsJudgeIndex,

    // kluby https://szts.ksis.eu/menu.php?akcia=CZK
    clubIndex: {
      mode: 'text',
      revalidatePeriod: '1 day',
      buildRequest: () => ({ url: new URL('https://szts.ksis.eu/menu.php?akcia=CZK') }),
      async load() {},
    } satisfies HtmlLoader,

    // solo https://szts.ksis.eu/menu.php?akcia=CZSD
    soloIndex: sztsSoloIndex,

    // páry https://szts.ksis.eu/menu.php?akcia=CZP
    coupleIndex: sztsCoupleIndex,

    // soutěže porotce https://szts.ksis.eu/rozhodca.php?meno=R%C3%B3bert%20Pavl%C3%ADk&mesto=Adamovsk%C3%A9%20Kochanovce
    // detail https://szts.ksis.eu/detail_paru.php?cp=12278
    // seznam výsledků https://szts.ksis.eu/menu.php?akcia=S&rok=2024
    // přihlášení https://szts.ksis.eu/zoznam_prihl.php?id_prop=2296
    // propozice https://szts.ksis.eu/prop.php?id_prop=2296
    // https://szts.ksis.eu/sutaz.php?sutaz_id=10532
    // https://szts.ksis.eu/hodnot_sut.php?sutaz_id=10525
    // => kategorie věkové/výkonnostní/licence = odvozené
  },
  pzst: {
    // https://baza.polskitaniec.org/cal/ = JSON prop in HTML is list of upcoming
    // https://baza.polskitaniec.org/api/competition/list/archive
    // https://baza.taniec-nowoczesny.pl/api/competition/list/archive
    // https://baza.fts-taniec.pl/x/api/competition/list/archive
    // https://baza.taniec-nowoczesny.pl/reg/LIVE/2025/20251108_Elblag_ZTN/competition.json?_cb=1764631900069
    // https://baza.taniec-nowoczesny.pl/reg/LIVE/2025/20251108_Elblag_ZTN/PZST_category_60001159.json?_cb=1764631931521
  },
};

type Loaders = typeof LOADERS;

export type LoaderIds = {
  [F in keyof Loaders]: {
    federation: F;
    kind: keyof (Loaders)[F];
  };
}[keyof Loaders];

export type LoaderEntry<I extends LoaderIds = LoaderIds> =
  I extends { federation: infer F; kind: infer K }
    ? F extends keyof Loaders
      ? K extends keyof Loaders[F]
        ? {
            federation: F;
            kind: K;
            loader: Loaders[F][K];
          }
        : never
      : never
    : never;

const entries = <T extends object>(value: T) =>
  Object.entries(value) as {
    [K in Extract<keyof T, string>]: [K, T[K]];
  }[Extract<keyof T, string>][];

export const ALL_LOADERS = entries(LOADERS)
  .flatMap(<F extends keyof Loaders & string>([federation, kinds]: [F, Loaders[F]]) =>
    entries(kinds).map(
      ([kind, loader]) => ({ federation, kind, loader })
    ) as LoaderEntry<Extract<LoaderIds, { federation: F }>>[]
  ) satisfies LoaderEntry[];

export function loaderFor(federation: string, kind: string): Loader | undefined {
  const LOADER_MAP: Record<string, Record<string, Loader>> = LOADERS;
  return LOADER_MAP[federation]?.[kind];
}

export function loadersFor(federation?: string | null, kind?: string | null): {
  federation: string; kind: string; loader: Loader
}[] {
  if (!federation) return ALL_LOADERS;
  if (!kind) return ALL_LOADERS.filter(x => x.federation === federation);
  const loader = loaderFor(federation, kind);
  return loader ? [{ federation, kind, loader } as any] : [];
}
