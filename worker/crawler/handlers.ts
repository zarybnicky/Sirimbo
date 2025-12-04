import { cstsAthlete } from './cstsAthlete.ts';
import type { HtmlLoader, JsonLoader } from './types.ts';

export const LOADERS: Record<string, Record<string, JsonLoader | HtmlLoader>> = {
  wdsf: {
    // modifiedsince, worldranking
    // computed merging of competitions into events, by location + same/adjacent date???
    // scraping by week, maybe?, or maybe from HTML
    // https://www.worlddancesport.org/Calendar/Competitions?Month=2&Year=2026 => /Events/Voesendorf-Vienna-Austria-21022026-22022026-8685
    // https://services.worlddancesport.org/api/1/competition?from=2025/11/01
    // https://services.worlddancesport.org/api/1/competition/64306
    // https://services.worlddancesport.org/api/1/participant?competitionId=64306
    // https://services.worlddancesport.org/api/1/official?competitionId=64306 - includes letters of judges
    // https://services.worlddancesport.org/api/1/person
    // https://services.worlddancesport.org/api/1/person/10116109
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
    member: cstsAthlete,

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
    // https://www.csts.cz/api/1/events/327/competitions?filter=eventCompetitionId%3D327&order=type
    // https://www.csts.cz/api/1/events/327/officials
    // https://www.csts.cz/api/1/events/327/competitors?eventCompetitionId=327

    // Výsledky
    // https://www.csts.cz/api/1/competition_events?from=2025-11-01&to=2025-11-30
    // https://www.csts.cz/api/1/competition_events/1262
    // https://www.csts.cz/api/1/competitions/32329/result

    // Ranklisty
    // https://www.csts.cz/api/1/ranklist
    // https://www.csts.cz/api/1/ranklist/6733
  },
  szts: {
    // soutěže https://szts.ksis.eu/menu.php?akcia=KS
    competitionIndex: {
      mode: 'text',
      revalidatePeriod: '1 day',
      buildRequest: ()=>  ({ url: new URL('https://szts.ksis.eu/menu.php?akcia=KS') }),
      async load() {},
    },
    // clenove https://szts.ksis.eu/menu.php?akcia=CZ
    memberIndex: {
      mode: 'text',
      revalidatePeriod: '1 day',
      buildRequest: ()=>  ({ url: new URL('https://szts.ksis.eu/menu.php?akcia=CZ') }),
      async load() {},
    },
    // trenéři https://szts.ksis.eu/menu.php?akcia=CZT
    trainerIndex: {
      mode: 'text',
      revalidatePeriod: '1 day',
      buildRequest: ()=>  ({ url: new URL('https://szts.ksis.eu/menu.php?akcia=CZT') }),
      async load() {},
    },
    // funkcionáři https://szts.ksis.eu/menu.php?akcia=CZS
    officialIndex: {
      mode: 'text',
      revalidatePeriod: '1 day',
      buildRequest: ()=>  ({ url: new URL('https://szts.ksis.eu/menu.php?akcia=CZS') }),
      async load() {},
    },
    // porotci https://szts.ksis.eu/menu.php?akcia=CZR
    judgeIndex: {
      mode: 'text',
      revalidatePeriod: '1 day',
      buildRequest: ()=>  ({ url: new URL('https://szts.ksis.eu/menu.php?akcia=CZR') }),
      async load() {},
    },
    // kluby https://szts.ksis.eu/menu.php?akcia=CZK
    clubIndex: {
      mode: 'text',
      revalidatePeriod: '1 day',
      buildRequest: ()=>  ({ url: new URL('https://szts.ksis.eu/menu.php?akcia=CZK') }),
      async load() {},
    },
    // solo https://szts.ksis.eu/menu.php?akcia=CZSD
    soloIndex: {
      mode: 'text',
      revalidatePeriod: '1 day',
      buildRequest: ()=>  ({ url: new URL('https://szts.ksis.eu/menu.php?akcia=CZSD') }),
      async load() {},
    },
    // páry https://szts.ksis.eu/menu.php?akcia=CZP
    coupleIndex: {
      mode: 'text',
      revalidatePeriod: '1 day',
      buildRequest: ()=>  ({ url: new URL('https://szts.ksis.eu/menu.php?akcia=CZP') }),
      async load() {},
    },
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
