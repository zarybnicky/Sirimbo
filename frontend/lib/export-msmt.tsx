import { saveAs } from 'file-saver';
import format from 'date-fns/format';
import { fetchGql } from './query';
import { MsmtExportDocument } from '@app/graphql/User';

export async function exportMSMT() {
  const { Workbook } = await import('exceljs');
  const data = await fetchGql(MsmtExportDocument, {});
  const workbook = new Workbook();
  const worksheet = workbook.addWorksheet('MŠMT Export');

  worksheet.columns = [
    { header: 'JMENO', key: 'firstName' },
    { header: 'PRIJMENI', key: 'lastName' },
    { header: 'TITUL_PRED', key: 'prefixTitle' },
    { header: 'TITUL_ZA', key: 'suffixTitle' },
    { header: 'RODNE_CISLO', key: 'birthNumber' },
    { header: 'OBCANSTVI', key: 'nationality' },
    { header: 'DATUM_NAROZENI', key: 'birthDate' },
    { header: 'NAZEV_OBCE', key: 'city' },
    { header: 'NAZEV_CASTI_OBCE', key: 'district' },
    { header: 'NAZEV_ULICE', key: 'street' },
    { header: 'CISLO_POPISNE', key: 'conscriptionNumber' },
    { header: 'CISLO_ORIENTACNI', key: 'orientationNumber' },
    { header: 'PSC', key: 'postalCode' },
    { header: 'SPORTOVEC', key: 'isAthlete' },
    { header: 'SPORTOVCEM_OD', key: 'athleteSince' },
    { header: 'SPORTOVCEM_DO', key: 'athleteUntil' },
    { header: 'SPORTOVEC_CETNOST', key: 'athleteWeeklyActivity' },
    { header: 'SPORTOVEC_DRUH_SPORTU', key: 'sportType' },
    { header: 'SPORTOVEC_UCAST_SOUTEZE', key: 'isCompeting' },
    { header: 'TRENER', key: 'isTrainer' },
    { header: 'TRENEREM_OD', key: 'trainerSince' },
    { header: 'TRENEREM_DO', key: 'trainerUntil' },
    { header: 'TRENER_CETNOST', key: 'trainerWeeklyActivity' },
    { header: 'TRENER_DRUH_SPORTU', key: 'sportTypeTrainer' },
    { header: 'EXT_ID', key: 'extId' },
  ];

  worksheet.getRow(1).font = { bold: true };
  worksheet.columns.forEach((column) => {
    column.width = (column?.header?.length || 0) + 10;
    column.alignment = { horizontal: 'center' };
  });

  data.users?.nodes.forEach((x) => {
    worksheet.addRow({
      firstName: x.uJmeno,
      lastName: x.uPrijmeni,
      birthNumber: x.uRodneCislo,
      nationality: x.uNationality === '203' ? 'CZE' : x.uNationality,
      birthDate: x.uNarozeni ? format(new Date(x.uNarozeni), 'd.M.yyyy') : '',
      city: x.uCity,
      district: x.uDistrict,
      street: x.uStreet,
      conscriptionNumber: x.uConscriptionNumber,
      orientationNumber: x.uOrientationNumber,
      postalCode: x.uPostalCode?.replaceAll(' ', ''),
      isAthlete: x.uGroup === '3' ? '1' : '0',
      sportType: x.uGroup === '3' ? '66' : '',
      isCompeting: x.uGroup !== '3' ? '1' : '0',
      isTrainer: x.uGroup !== '3' ? '1' : '0',
      sportTypeTrainer: x.uGroup !== '3' ? '66' : '',
      athleteSince: x.dateOfOldestPayment ? format(new Date(x.dateOfOldestPayment), 'd.M.yyyy') : '',
      extId: x.id,
    });
  });

  const buf = await workbook.csv.writeBuffer();
  saveAs(new Blob([buf]), 'Olymp-MSMT-2023.csv');
}
