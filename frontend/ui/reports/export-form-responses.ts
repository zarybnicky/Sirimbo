import { saveAs } from 'file-saver';
import { FormResponsesDocument } from '@/graphql/Crm';
import { fetchGql } from '@/graphql/query';

export async function exportFormResponses() {
  const data = await fetchGql(FormResponsesDocument, {});
  const nodes = data?.formResponses?.nodes || [];

  const { Workbook } = await import('exceljs');
  const workbook = new Workbook();
  const worksheet = workbook.addWorksheet('Zájemci');

  worksheet.columns = [
    { header: 'Jméno', key: 'name' },
    { header: 'Přijmení', key: 'surname' },
    { header: 'E-mail', key: 'email' },
    { header: 'Telefon', key: 'phone' },
    { header: 'Narození', key: 'born' },
    { header: 'Zdroj', key: 'source' },
    { header: 'Poslední aktivita', key: 'createdAt' },
  ];

  worksheet.getRow(1).font = { bold: true };
  worksheet.columns.forEach((column) => {
    column.width = (column?.header?.length || 0) + 10;
    column.alignment = { horizontal: 'center' };
  });

  nodes.forEach((x) =>
    worksheet.addRow({
      name: x.data.name,
      surname: x.data.surname,
      email: x.data.email,
      phone: x.data.phone,
      born: x.data.yearofbirth,
      source: x.url,
      createdAt: x.createdAt
               ? new Date(x.createdAt).toISOString().substring(0, 10)
               : '',
    }),
  );

  const buf = await workbook.xlsx.writeBuffer();
  saveAs(
    new Blob([buf]),
    `${new Date().toISOString().substring(0, 10)}-Zájemci.xlsx`,
  );
};
