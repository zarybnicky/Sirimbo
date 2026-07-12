import { EventInstanceExportDocument } from '@/graphql/Event';
import { formatEventType, fullDateFormatter } from '@/ui/format';
import { saveAs } from 'file-saver';
import type { Client } from 'urql';

export async function exportEventRegistrations(client: Client, id: string) {
  const result = await client.query(EventInstanceExportDocument, { id }).toPromise();
  if (result.error) throw result.error;
  const instance = result.data?.eventInstance;
  const { Workbook } = await import('exceljs');
  const workbook = new Workbook();
  const name = instance?.name || formatEventType(instance?.type) || 'Sheet 1';
  const worksheet = workbook.addWorksheet(name);

  const columns = [
    { header: 'Partner', key: 'man' },
    { header: 'Partnerka', key: 'woman' },
    { header: 'Datum přihlášení', key: 'registered' },
    { header: 'Poznámka', key: 'note' },
  ];
  const lessonTrainers = new Map<string, string>();
  for (const trainer of instance?.trainersList ?? []) {
    if (trainer.lessonsOffered !== 0) {
      lessonTrainers.set(trainer.personId, trainer.person?.name ?? '?');
    }
  }
  for (const registration of instance?.registrations ?? []) {
    for (const demand of registration.eventLessonDemandsByRegistrationIdList) {
      if (demand.trainer?.person) {
        lessonTrainers.set(demand.trainer.person.id, demand.trainer.person.name ?? '?');
      }
    }
  }
  for (const [id, name] of lessonTrainers) {
    columns.push({
      header: name,
      key: id,
    });
  }
  worksheet.columns = columns;

  worksheet.getRow(1).font = { bold: true };
  for (const column of worksheet.columns) {
    column.width = (column?.header?.length || 0) + 10;
    column.alignment = { horizontal: 'center' };
  }

  const rows: { [k: string]: string }[] = [];
  for (const x of instance?.registrations || []) {
    const row: { [key: string]: string } = {
      man: x.person?.name || x.couple?.man?.name || '',
      woman: x.couple?.woman?.name || '',
      registered: fullDateFormatter.format(new Date(x.createdAt)),
      note: x.note || '',
    };
    for (const demand of x.eventLessonDemandsByRegistrationIdList) {
      const trainerId = demand.trainer?.person?.id;
      if (trainerId) {
        row[trainerId] = String(Number(row[trainerId] ?? 0) + demand.lessonCount);
      }
    }
    rows.push(row);
  }
  for (const x of instance?.eventExternalRegistrationsByInstanceIdList || []) {
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
