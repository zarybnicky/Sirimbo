import { PersonListDocument } from '@/graphql/Person';
import { fetchGql } from '@/graphql/query';
import { fullDateFormatter } from '@/ui/format';
import { saveAs } from 'file-saver';

export async function exportCohort(ids: string[], name?: string) {
  const { Workbook } = await import('exceljs');
  const data = await fetchGql(PersonListDocument, { inCohorts: ids });
  const workbook = new Workbook();
  const worksheet = workbook.addWorksheet(name || 'Sheet 1');

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

  for (const x of data.filteredPeopleList || []) {
    worksheet.addRow({
      firstName: x.firstName,
      lastName: x.lastName,
      birthNumber: x.taxIdentificationNumber,
      birthDate: x.birthDate ? fullDateFormatter.format(new Date(x.birthDate)) : '',
      phone: x.phone,
      email: x.email,
      cohorts: x.cohortMembershipsList.map(x => x.cohort?.name).join(', '),
    });
  }

  const buf = await workbook.xlsx.writeBuffer();
  saveAs(new Blob([buf]), `${name || 'Všechny skupiny'}.xlsx`);
};
