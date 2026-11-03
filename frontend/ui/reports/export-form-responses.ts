import { saveAs } from 'file-saver';
import { FormResponsesDocument } from '@/graphql/Crm';
import type { Client } from 'urql';

export async function exportFormResponses(client: Client) {
  const result = await client.query(FormResponsesDocument, {}).toPromise();
  if (result.error) throw result.error;
  const nodes = result.data?.formResponses?.nodes ?? [];

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
  for (const column of worksheet.columns) {
    column.width = (column?.header?.length || 0) + 10;
    column.alignment = { horizontal: 'center' };
  }

  for (const { type, data, url, createdAt } of nodes) {
    if (type === 'Zpětná vazba, web 05/2023') continue;
    worksheet.addRow({
      name: data.name,
      surname: data.surname,
      email: data.email,
      phone: data.phone,
      born: data.yearofbirth,
      source: url,
      createdAt: createdAt ? new Date(createdAt).toISOString().slice(0, 10) : '',
    });
  }

  const buf = await workbook.xlsx.writeBuffer();
  saveAs(new Blob([buf]), `${new Date().toISOString().slice(0, 10)}-Zájemci.xlsx`);
}
