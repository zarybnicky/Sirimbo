import { PersonWithLinksListDocument } from '@/graphql/Person';
import { fetchGql } from '@/graphql/query';
import { saveAs } from 'file-saver';
import { stringify } from 'csv-stringify/sync';

export async function exportNsa() {
  const data = await fetchGql(PersonWithLinksListDocument, {});

  const inputs = (data.filteredPeopleList || []).filter(x => x.isMember || x.isTrainer);
  inputs.sort((x, y) => `${x.isTrainer} ${x.lastName} ${x.firstName}`.localeCompare(`${y.isTrainer} ${y.lastName} ${y.firstName}`));

  const initDate = new Date();

  const rows: Record<string, string>[] = [];
  for (const x of inputs) {
    const athleteSince = x.tenantMembershipsList
      .map(y => new Date(y.since))
      .reduceRight((min, y) => y < min ? y : min, initDate);
    const trainerSince = x.tenantTrainersList
      .map(y => new Date(y.since))
      .reduceRight((min, y) => y < min ? y : min, initDate);

    rows.push({
      "[JMENO]": x.firstName,
      "[PRIJMENI]": x.lastName,
      "[TITUL_PRED]": x.prefixTitle,
      "[TITUL_ZA]": x.suffixTitle,
      "[RODNE_CISLO]": x.nationality === '203' ? x.taxIdentificationNumber || '' : '',
      "[OBCANSTVI]": {
        203: 'CZE',
        705: 'SVN',
        804: 'UKR',
        428: 'LTU',
        703: 'SVK',
        348: 'HUN',
      }[x.nationality] || '',
      "[DATUM_NAROZENI]": x.nationality !== '203' ? formatNsaDate(x.birthDate) : '',
      "[POHLAVI]": x.gender === 'MAN' ? 'M' : 'Ž',
      "[NAZEV_OBCE]": x.address?.city || '',
      "[NAZEV_CASTI_OBCE]": x.address?.district || '',
      "[NAZEV_ULICE]": x.address?.street || '',
      "[CISLO_POPISNE]": x.address?.conscriptionNumber || '',
      "[CISLO_ORIENTACNI]": x.address?.orientationNumber || '',
      "[PSC]": x.address?.postalCode || '',
      "[SPORTOVEC]": x.tenantMembershipsList.some(y => y.active) ? '1' : '0',
      "[SPORTOVCEM_OD]": athleteSince == initDate ? '' : formatNsaDate(athleteSince.toString()),
      "[SPORTOVCEM_DO]": '',
      "[SPORTOVEC_CETNOST]": "3",
      "[SPORTOVEC_DRUH_SPORTU]": '66.1',
      "[SPORTOVEC_UCAST_SOUTEZE_POCET]": '6',
      "[TRENER]": x.tenantTrainersList.some(y => y.active) ? '1' : '0',
      "[TRENEREM_OD]": trainerSince == initDate ? '' : formatNsaDate(trainerSince.toString()),
      "[TRENEREM_DO]": '',
      "[TRENER_CETNOST]": "3",
      "[TRENER_DRUH_SPORTU]": '66.1',
      "[EXT_ID]": x.cstsId || '',
      "[SVAZ_ICO_SKTJ]": '',
      "[STAV]": '',
    });
  }

  const buf = stringify(rows, {
    header: true,
    bom: true,
    delimiter: ';',
  });
  saveAs(new Blob([new TextEncoder().encode(buf)]), 'NSA-export.csv');
}

function formatNsaDate(dateString: string | null) {
  if (!dateString) return '';
  const date = new Date(dateString);
  return `${date.getDate()}.${date.getMonth() + 1}.${date.getFullYear()}`;
}
