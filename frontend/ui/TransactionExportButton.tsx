import { formatDefaultEventName, formatEventType } from '@/ui/format';
import { PostingFragment } from '@/graphql/Payment';
import { saveAs } from 'file-saver';
import { buttonCls } from './style';

export async function exportPostings(name: string, postings: PostingFragment[]) {
  const { Workbook } = await import('exceljs');
  const workbook = new Workbook();
  const worksheet = workbook.addWorksheet(name);
  let sum = 0.0;
  worksheet.columns = [
    { header: 'Datum', key: 'date' },
    { header: 'Popis', key: 'desc' },
    { header: 'Částka', key: 'amount' },
  ];
  worksheet.getRow(1).font = { bold: true };
  worksheet.columns.forEach((column) => {
    column.width = (column?.header?.length || 0) + 30;
  });

  const processed = postings.map((x) => {
    const payment = x.transaction?.payment;

    let date = x?.transaction?.effectiveDate;
    let desc = '';

    let event = payment?.eventInstance?.event
    if (event) {
      desc = parseFloat(x.amount) < 0 ? ((formatEventType(event) + ': ') + event.eventTrainersList.map(x => x.person?.name).join(', ')) : formatDefaultEventName(event);
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
      desc = `Příspěvky: ${cohort.sName}`;
    }

    sum += Math.round(parseFloat(x.amount) * 100) / 100;
    return {
      date: date ? new Date(date).toISOString() : '',
      desc,
      amount: `${Math.round(parseFloat(x.amount) * 100) / 100}`,
    };
  }).sort((a, b) => a.date.localeCompare(b.date));

  processed.forEach(x => {
    worksheet.addRow(x);
  });

  worksheet.addRow({});
  worksheet.addRow({
    amount: sum,
  });

  const buf = await workbook.xlsx.writeBuffer();
  saveAs(new Blob([buf]), name + '.xlsx');
}

export function TransactionExportButton({ name, postings }: {
  name: string;
  postings: PostingFragment[];
}) {
  return (
    <button className={buttonCls()} onClick={() => exportPostings(`${new Date().getFullYear()}-${new Date().getMonth()} ${name}`, postings)}>Export XLSX</button>
  );
}
