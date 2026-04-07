import { PersonWithLinksListDocument } from '@/graphql/Person';
import { saveAs } from 'file-saver';
import { stringify } from 'csv-stringify/sync';
import type { Client } from 'urql';

export async function exportNsa(client: Client) {
  const result = await client.query(PersonWithLinksListDocument, {}).toPromise();
  if (result.error) throw result.error;
  const data = result.data!;

  const inputs = (data.filteredPeopleList || []).filter((x) => x.isMember || x.isTrainer);
  inputs.sort((x, y) =>
    `${x.isTrainer} ${x.lastName} ${x.firstName}`.localeCompare(
      `${y.isTrainer} ${y.lastName} ${y.firstName}`,
    ),
  );

  const initDate = Date.now();

  const rows: Record<string, string>[] = [];
  for (const x of inputs) {
    const isTrainer = x.isTrainer;
    const isAthlete = x.cohortMembershipsList.some(x => x.cohort?.isVisible);
    const isCzech = x.nationality === '203';

    if (!isTrainer && !isAthlete)
      continue;

    const athleteSince = x.tenantMembershipsList
      .map((y) => Date.parse(y.since))
      .reduceRight((min, y) => Math.min(y, min), initDate);
    const trainerSince = x.tenantTrainersList
      .map((y) => Date.parse(y.since))
      .reduceRight((min, y) => Math.min(y, min), initDate);

    rows.push({
      '[JMENO]': x.firstName,
      '[PRIJMENI]': x.lastName,
      '[TITUL_PRED]': x.prefixTitle,
      '[TITUL_ZA]': x.suffixTitle,
      '[RODNE_CISLO]': isCzech ? x.taxIdentificationNumber || '' : '',
      '[OBCANSTVI]':
        {
          203: 'CZE',
          705: 'SVN',
          804: 'UKR',
          428: 'LTU',
          703: 'SVK',
          348: 'HUN',
        }[x.nationality] || '',
      '[DATUM_NAROZENI]': (isCzech || !x.birthDate) ? '' : formatNsaDate(Date.parse(x.birthDate)),
      '[POHLAVI]': x.gender === 'MAN' ? 'M' : 'Ž',
      '[NAZEV_OBCE]': isCzech ? '' : x.address?.city || '',
      '[NAZEV_CASTI_OBCE]': isCzech ? '' : x.address?.district || '',
      '[NAZEV_ULICE]': isCzech ? '' : x.address?.street || '',
      '[CISLO_POPISNE]': isCzech ? '' : x.address?.conscriptionNumber || '',
      '[CISLO_ORIENTACNI]': isCzech ? '' : x.address?.orientationNumber || '',
      '[PSC]': isCzech ? '' : x.address?.postalCode || '',
      '[SPORTOVEC]': isAthlete ? '1' : '0',
      '[SPORTOVCEM_OD]': isAthlete ? formatNsaDate(athleteSince) : '',
      '[SPORTOVCEM_DO]': '',
      '[SPORTOVEC_CETNOST]': isAthlete ? '3' : '',
      '[SPORTOVEC_DRUH_SPORTU]': isAthlete ? '66.1' : '',
      '[SPORTOVEC_UCAST_SOUTEZE_POCET]': isAthlete ? '6' : '',
      '[TRENER]': x.isTrainer ? '1' : '0',
      '[TRENEREM_OD]': x.isTrainer ? formatNsaDate(trainerSince) : '',
      '[TRENEREM_DO]': '',
      '[TRENER_CETNOST]': x.isTrainer ? '3' : '',
      '[TRENER_DRUH_SPORTU]': x.isTrainer ? '66.1' : '',
      '[EXT_ID]': x.cstsId || '',
      '[SVAZ_ICO_SKTJ]': '',
    });
  }

  const buf = stringify(rows, {
    header: true,
    bom: true,
    delimiter: ';',
  });
  saveAs(new Blob([new TextEncoder().encode(buf)]), 'NSA-export.csv');
}

function formatNsaDate(dateString: number | null) {
  if (!dateString) return '';
  const date = new Date(dateString);
  return `${date.getDate()}.${date.getMonth() + 1}.${date.getFullYear()}`;
}
