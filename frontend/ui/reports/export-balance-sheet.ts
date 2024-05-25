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
    { header: 'Příchozí', key: 'assets' },
    { header: 'Odchozí', key: 'liabilities' },
    { header: 'Stav k 31.12.2023', key: 'balance' },
  ];
  worksheet.getRow(1).font = { bold: true };
  worksheet.columns.forEach((column) => {
    column.width = (column?.header?.length || 0) + 30;
  });

  const processed = (data?.accountsList || [])
    .map((x) => {
      const assets = 100 * Math.round(parseFloat(x.assets) / 100);
      const liabilities = 100 * Math.round(parseFloat(x.liabilities) / 100);
      return {
        name: x.person?.name || '',
        firstName: x.person?.firstName || '',
        lastName: x.person?.lastName || '',
        assets: assets,
        liabilities: liabilities,
        balance: assets + liabilities,
      };
    })
    .filter(x => !!x.name)
    .sort((a, b) => `${a.lastName}${a.firstName}`.localeCompare(`${b.lastName}${b.firstName}`));

  processed.forEach((x) => {
    worksheet.addRow(x);
  });

  const buf = await workbook.xlsx.writeBuffer();
  saveAs(new Blob([buf]), 'Přehled plateb 2023.xlsx');
}
