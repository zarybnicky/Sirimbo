import { BalanceSheetDocument } from '@/graphql/Payment';
import { saveAs } from 'file-saver';
import { fetchGql } from '@/graphql/query';

export async function exportBalanceSheet() {
  const { Workbook } = await import('exceljs');
  const data = await fetchGql(BalanceSheetDocument, {
    since: new Date('2023-09-01').toISOString(),
    until: new Date('2023-12-31').toISOString(),
  });

  const workbook = new Workbook();
  const worksheet = workbook.addWorksheet('Přehled plateb 2023');
  worksheet.columns = [
    { header: 'Člen', key: 'name' },
    { header: 'Příchozí', key: 'assets', numFmt: '#,##0.00 Kč' },
    { header: 'Odchozí', key: 'liabilities', numFmt: '#,##0.00 Kč' },
    { header: 'Stav k 31.12.2023', key: 'balance' },
  ];
  worksheet.getRow(1).font = { bold: true };
  for (const column of worksheet.columns) {
    column.width = (column?.header?.length || 0) + 30;
  }

  const accounts = (data?.accountsList || [])
    .filter(x => x.person?.name)
    .sort((a, b) => `${a.person?.lastName}${a.person?.firstName}`.localeCompare(`${b.person?.lastName}${b.person?.firstName}`));

  for (const x of accounts) {
    const assets = 100 * Math.round(Number.parseFloat(x.assets) / 100);
    const liabilities = 100 * Math.round(Number.parseFloat(x.liabilities) / 100);
    worksheet.addRow({
      name: x.person?.name || '',
      assets: assets,
      liabilities: liabilities,
      balance: assets + liabilities,
    });
  }

  const buf = await workbook.xlsx.writeBuffer();
  saveAs(new Blob([buf]), 'Přehled plateb 2023.xlsx');
}
