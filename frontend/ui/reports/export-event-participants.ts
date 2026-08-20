import {
  EventExportDocument,
  type EventRegistrantFragment,
} from '@/graphql/Event';
import { formatEventName, fullDateFormatter } from '@/ui/format';
import { saveAs } from 'file-saver';
import type { Client } from 'urql';

export async function exportEventParticipants(client: Client, id: string) {
  const result = await client.query(EventExportDocument, { id }).toPromise();
  if (result.error) throw result.error;
  const { event } = result.data ?? {};
  if (!event) throw new Error("Událost nenalezena");

  const { Workbook } = await import('exceljs');
  const workbook = new Workbook();
  const name = event.name || formatEventName(event) || 'Sheet 1';
  const worksheet = workbook.addWorksheet(name);

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

  const personIds = new Set<string>();
  const addPerson = (x: EventRegistrantFragment | null | undefined) => {
    if (!x || personIds.has(x.id)) return;
    personIds.add(x.id);
    worksheet.addRow({
      firstName: x.firstName,
      lastName: x.lastName,
      birthNumber: x.taxIdentificationNumber,
      birthDate: x.birthDate ? fullDateFormatter.format(new Date(x.birthDate)) : '',
      phone: x.phone,
      email: x.email,
      cohorts: x.cohortMembershipsList.map((x) => x.cohort?.name).join(', '),
    });
  };
  for (const registration of event.registrations.nodes) {
    addPerson(registration.person);
    addPerson(registration.couple?.man);
    addPerson(registration.couple?.woman);
  }
  for (const x of event?.externalRegistrations || []) {
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
  saveAs(new Blob([buf]), `${name}.xlsx`);
}
