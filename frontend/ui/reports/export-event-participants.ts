import { EventRegistrantsDocument } from '@/graphql/Event';
import { fetchGql } from '@/graphql/query';
import { fullDateFormatter } from '@/ui/format';
import { saveAs } from 'file-saver';

export async function exportEventParticipants(id: string) {
  const data = await fetchGql(EventRegistrantsDocument, { id });
  const { Workbook } = await import('exceljs');
  const workbook = new Workbook();
  const worksheet = workbook.addWorksheet(data.event?.name || 'Sheet 1');

  worksheet.columns = [
    { header: 'Jméno', key: 'firstName' },
    { header: 'Přijmení', key: 'lastName' },
    { header: 'Rodné číslo', key: 'birthNumber' },
    { header: 'Datum narození', key: 'birthDate' },
    { header: 'Telefon', key: 'phone' },
    { header: 'E-mail', key: 'email' },
    { header: 'Skupiny', key: 'cohorts' },
  ];

  worksheet.getRow(1).font = { bold: true };
  for (const column of worksheet.columns) {
    column.width = (column?.header?.length || 0) + 10;
    column.alignment = { horizontal: 'center' };
  }

  for (const x of data.event?.registrantsList || []) {
    worksheet.addRow({
      firstName: x.firstName,
      lastName: x.lastName,
      birthNumber: x.taxIdentificationNumber,
      birthDate: x.birthDate ? fullDateFormatter.format(new Date(x.birthDate)) : '',
      phone: x.phone,
      email: x.email,
      cohorts: x.cohortMembershipsList.map((x) => x.cohort?.name).join(', '),
    });
  }
  for (const x of data.event?.eventExternalRegistrationsList || []) {
    worksheet.addRow({
      firstName: x.firstName,
      lastName: x.lastName,
      birthNumber: x.taxIdentificationNumber,
      birthDate: x.birthDate ? fullDateFormatter.format(new Date(x.birthDate)) : '',
      phone: x.phone,
      email: x.email,
      cohorts: '',
    });
  }

  const buf = await workbook.xlsx.writeBuffer();
  saveAs(new Blob([buf]), `${data.event?.name || 'export-akce'}.xlsx`);
}
