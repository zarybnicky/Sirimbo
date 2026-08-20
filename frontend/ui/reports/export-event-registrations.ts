import { EventExportDocument } from '@/graphql/Event';
import { formatEventName, fullDateFormatter } from '@/ui/format';
import { saveAs } from 'file-saver';
import type { Client } from 'urql';

export async function exportEventRegistrations(client: Client, id: string) {
  const result = await client.query(EventExportDocument, { id }).toPromise();
  if (result.error) throw result.error;
  const { event } = result.data ?? {};
  if (!event) throw new Error("Událost nenalezena");

  const { Workbook } = await import('exceljs');
  const workbook = new Workbook();
  const name = event.name || formatEventName(event) || 'Sheet 1';
  const worksheet = workbook.addWorksheet(name);

  const columns = [
    { header: 'Partner', key: 'man' },
    { header: 'Partnerka', key: 'woman' },
    { header: 'Datum přihlášení', key: 'registered' },
    { header: 'Poznámka', key: 'note' },
  ];
  const lessonTrainers = new Map<string, string>();
  for (const trainer of event.trainersList ?? []) {
    if (trainer.lessonsOffered !== 0) {
      lessonTrainers.set(trainer.personId, trainer.person?.name ?? '?');
    }
  }
  for (const registration of event.registrations.nodes) {
    for (const request of registration.requests) {
      if (request.trainer?.personId) {
        lessonTrainers.set(request.trainer.personId, request.trainer.person?.name ?? '?');
      }
    }
  }
  for (const [key, header] of lessonTrainers) {
    columns.push({ header, key });
  }
  worksheet.columns = columns;

  worksheet.getRow(1).font = { bold: true };
  for (const column of worksheet.columns) {
    column.width = (column?.header?.length || 0) + 10;
    column.alignment = { horizontal: 'center' };
  }

  const rows: { [k: string]: string }[] = [];
  for (const x of event.registrations.nodes) {
    const row: { [key: string]: string } = {
      man: x.person?.name || x.couple?.man?.name || '',
      woman: x.couple?.woman?.name || '',
      registered: fullDateFormatter.format(new Date(x.createdAt)),
      note: x.note || '',
    };
    for (const request of x.requests) {
      const trainerId = request.trainer?.personId;
      if (trainerId) {
        row[trainerId] = String(Number(row[trainerId] ?? 0) + request.lessonCount);
      }
    }
    rows.push(row);
  }
  for (const x of event.externalRegistrations || []) {
    const row: { [key: string]: string } = {
      man: `${x.prefixTitle} ${x.firstName} ${x.lastName} ${x.suffixTitle}`,
      woman: '',
      registered: fullDateFormatter.format(new Date(x.createdAt)),
      note: x.note || '',
    };
    rows.push(row);
  }
  rows.sort((a, b) => (a.man as string).localeCompare(b.man as string));
  worksheet.addRows(rows);

  const buf = await workbook.xlsx.writeBuffer();
  saveAs(new Blob([buf]), `${name}.xlsx`);
}
