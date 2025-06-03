import { formatDefaultEventName, formatEventType } from '@/ui/format';
import type { PostingFragment } from '@/graphql/Payment';
import { saveAs } from 'file-saver';

export async function exportPostings(name: string, postings: PostingFragment[]) {
  const { Workbook } = await import('exceljs');
  const workbook = new Workbook();
  const worksheet = workbook.addWorksheet(name);
  let sum = 0;
  worksheet.columns = [
    { header: 'Datum', key: 'date' },
    { header: 'Popis', key: 'desc' },
    { header: 'Částka', key: 'amount', numFmt: '#,##0.00 Kč' },
  ];
  worksheet.getRow(1).font = { bold: true };
  for (const column of worksheet.columns) {
    column.width = (column?.header?.length || 0) + 30;
  };

  const processed = postings.map((x) => {
    const payment = x.transaction?.payment;

    let date = x?.transaction?.effectiveDate;
    let desc = '';

    let event = payment?.eventInstance?.event
    if (event) {
      desc = Number.parseFloat(x.amount) < 0 ? `${formatEventType(event)}: ${event.eventTrainersList.map(x => x.name).join(', ')}` : formatDefaultEventName(event);
      date = payment?.eventInstance?.since
    }

    event = payment?.eventRegistration?.event;
    if (event) {
      desc = formatDefaultEventName(event);
      date = event.eventInstancesList?.[0]?.since;
    }

    const cohort = payment?.cohortSubscription?.cohort
    if (cohort) {
      date = payment?.dueAt || date;
      desc = `Příspěvky: ${cohort.name}`;
    }

    sum += Math.round(Number.parseFloat(x.amount) * 100) / 100;
    return {
      date: date ? new Date(date).toISOString() : '',
      desc,
      amount: `${Math.round(Number.parseFloat(x.amount) * 100) / 100}`,
    };
  }).sort((a, b) => a.date.localeCompare(b.date));

  worksheet.addRows(processed);
  worksheet.addRow({});
  worksheet.addRow({
    amount: sum,
  });

  const buf = await workbook.xlsx.writeBuffer();
  saveAs(new Blob([buf]), `${name}.xlsx`);
}
