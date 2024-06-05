import { EventDocument } from '@/graphql/Event';
import { fetchGql } from '@/graphql/query';
import { fullDateFormatter } from '@/ui/format';
import { saveAs } from 'file-saver';

export async function exportEventRegistrations(id: string) {
  const data = await fetchGql(EventDocument, { id });
  const { Workbook } = await import('exceljs');
  const workbook = new Workbook();
  const worksheet = workbook.addWorksheet(data.event?.name || 'Sheet 1');

  const columns = [
    { header: 'Partner', key: 'man' },
    { header: 'Partnerka', key: 'woman' },
    { header: 'Datum přihlášení', key: 'registered' },
    { header: 'Poznámka', key: 'note' },
  ];
  for (const trainer of data.event?.eventTrainersList || []) {
    columns.push({
      header: trainer.name || '?',
      key: trainer.id,
    })
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
      row[demand.trainerId] = demand.lessonCount.toString();
    }
    rows.push(row);
  }
  rows.sort((a, b) => (a.man as string).localeCompare(b.man as string))
  worksheet.addRows(rows);

  const buf = await workbook.xlsx.writeBuffer();
  saveAs(new Blob([buf]), `${data.event?.name || 'export-akce'}.xlsx`);
};
