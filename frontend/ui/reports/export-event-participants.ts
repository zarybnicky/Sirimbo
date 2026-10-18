import { EventParticipantsExportDocument } from '@/graphql/Event';
import { formatEventName, fullDateFormatter } from '@/ui/format';
import { saveAs } from 'file-saver';
import type { Client } from 'urql';

export async function exportEventParticipants(client: Client, id: string) {
  const result = await client.query(EventParticipantsExportDocument, { id }).toPromise();
  if (result.error) throw result.error;
  const { event } = result.data ?? {};
  if (!event) throw new Error("Událost nenalezena");

  const { Workbook } = await import('exceljs');
  const workbook = new Workbook();
  const name = event.name || formatEventName({
    ...event,
    registrations: {
      nodes: event.registrations.nodes.filter((x) => x.parentRegistrationId === null),
    },
  }) || 'Sheet 1';
  const worksheet = workbook.addWorksheet(name.replaceAll(/[^a-zA-Z0-9\s]/g, ''));

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

  for (const { person: x } of event.registrations.nodes) {
    if (!x) continue;
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

  const buf = await workbook.xlsx.writeBuffer();
  saveAs(new Blob([buf]), `${name}.xlsx`);
}
