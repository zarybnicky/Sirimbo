import { PersonWithLinksListDocument } from '@/graphql/Person';
import { fetchGql } from '@/lib/query';
import { saveAs } from 'file-saver';
import { stringify } from 'csv-stringify/sync';

export async function exportNsa() {
  const data = await fetchGql(PersonWithLinksListDocument, {});

  const inputs = (data.filteredPeopleList || []).filter((x) => x.isMember || x.isTrainer);
  inputs.sort((x, y) =>
    `${x.isTrainer} ${x.lastName} ${x.firstName}`.localeCompare(
      `${y.isTrainer} ${y.lastName} ${y.firstName}`,
    ),
  );

  const initDate = Date.now();

  const rows: Record<string, string>[] = [];
  for (const x of inputs) {
    if (x.cohortMembershipsList.every(x => !x.cohort?.isVisible) && !x.isTrainer)
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
      '[RODNE_CISLO]': x.nationality === '203' ? x.taxIdentificationNumber || '' : '',
      '[OBCANSTVI]':
        {
          203: 'CZE',
          705: 'SVN',
          804: 'UKR',
          428: 'LTU',
          703: 'SVK',
          348: 'HUN',
        }[x.nationality] || '',
      '[DATUM_NAROZENI]': (x.nationality === '203' || !x.birthDate) ? '' : formatNsaDate(Date.parse(x.birthDate)),
      '[POHLAVI]': x.gender === 'MAN' ? 'M' : 'Ž',
      '[NAZEV_OBCE]': x.nationality === '203' ? '' : x.address?.city || '',
      '[NAZEV_CASTI_OBCE]': x.nationality === '203' ? '' : x.address?.district || '',
      '[NAZEV_ULICE]': x.nationality === '203' ? '' : x.address?.street || '',
      '[CISLO_POPISNE]': x.nationality === '203' ? '' : x.address?.conscriptionNumber || '',
      '[CISLO_ORIENTACNI]': x.nationality === '203' ? '' : x.address?.orientationNumber || '',
      '[PSC]': x.nationality === '203' ? '' : x.address?.postalCode || '',
      '[SPORTOVEC]': !x.isTrainer ? '1' : '0',
      '[SPORTOVCEM_OD]': !x.isTrainer ? '' : formatNsaDate(athleteSince),
      '[SPORTOVCEM_DO]': '',
      '[SPORTOVEC_CETNOST]': !x.isTrainer ? '3' : '0',
      '[SPORTOVEC_DRUH_SPORTU]': '66.1',
      '[SPORTOVEC_UCAST_SOUTEZE_POCET]': '6',
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
