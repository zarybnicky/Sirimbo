import * as React from 'react';
import { PermissionKey, PermissionLevel } from 'lib/data/use-permissions';
import { fullDateFormatter } from 'lib/format-date';
import { Item } from 'components/layout/Item';
import { Button } from 'components/Button';
import { saveAs } from 'file-saver';
import { useFormResponsesQuery } from 'lib/graphql/Crm';
import { type NextPageWithLayout } from 'pages/_app';

const Page: NextPageWithLayout = () => {
  const { data } = useFormResponsesQuery();
  const saveData = React.useCallback(
    async (e?: React.MouseEvent) => {
      e?.preventDefault();

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
          name: x.data?.name,
          surname: x.data?.surname,
          email: x.data?.email,
          phone: x.data?.phone,
          born: x.data?.yearofbirth,
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
    },
    [data],
  );

  return (
    <Item className="col-feature">
      <Item.Titlebar title="Chci tančit">
        <Button onClick={saveData}>Export všech</Button>
      </Item.Titlebar>

      <table className="w-full text-sm text-left text-stone-900">
        <thead>
          <tr>
            <th>Jméno</th>
            <th>E-mail</th>
            <th>Telefon</th>
            <th>Narození</th>
            <th>Zdroj</th>
            <th>Poslední aktivita</th>
          </tr>
        </thead>
        <tbody>
          {data?.formResponses?.nodes?.map((row, i) => (
            <tr key={i} className="even:bg-white odd:bg-stone-200 border-b">
              <td className="py-1">
                {row.data?.name} {row.data?.surname}
              </td>
              <td>{row.data?.email}</td>
              <td>{row.data?.phone}</td>
              <td>{row.data?.yearofbirth}</td>
              <td>{row.type}</td>
              <td className="text-right">
                {row.createdAt ? fullDateFormatter.format(new Date(row.createdAt)) : ''}
              </td>
            </tr>
          ))}
        </tbody>
      </table>
    </Item>
  );
}

Page.permissions = [PermissionKey.peNastenka, PermissionLevel.P_ADMIN];

export default Page;
