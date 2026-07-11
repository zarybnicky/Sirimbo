import { EventDocument } from '@/graphql/Event';
import { fullDateFormatter } from '@/ui/format';
import { saveAs } from 'file-saver';
import type { Client } from 'urql';

export async function exportEventRegistrations(client: Client, id: string) {
  const result = await client.query(EventDocument, { id }).toPromise();
  if (result.error) throw result.error;
  const data = result.data!;
  const { Workbook } = await import('exceljs');
  const workbook = new Workbook();
  const worksheet = workbook.addWorksheet(data.event?.name || 'Sheet 1');

  const columns = [
    { header: 'Partner', key: 'man' },
    { header: 'Partnerka', key: 'woman' },
    { header: 'Datum přihlášení', key: 'registered' },
    { header: 'Poznámka', key: 'note' },
  ];
  const lessonTrainers = new Map(
    [
      ...(data.event?.eventInstancesList.flatMap((instance) =>
        instance.eventInstanceTrainersByInstanceIdList.flatMap((trainer) =>
          trainer.lessonsOffered !== 0
            ? [[trainer.personId, trainer.person?.name ?? '?'] as const]
            : [],
        ),
      ) ?? []),
      ...(data.event?.eventRegistrationsList.flatMap((registration) =>
        registration.eventLessonDemandsByRegistrationIdList.flatMap((demand) =>
          demand.trainer?.person
            ? [[demand.trainer.person.id, demand.trainer.person.name ?? '?'] as const]
            : [],
        ),
      ) ?? []),
    ],
  );
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
  for (const x of data.event?.eventRegistrationsList || []) {
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
  for (const x of data.event?.eventExternalRegistrationsList || []) {
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
  saveAs(new Blob([buf]), `${data.event?.name || 'export-akce'}.xlsx`);
}
