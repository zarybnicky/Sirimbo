import * as React from 'react';
import { fullDateFormatter } from '@/ui/format';
import { saveAs } from 'file-saver';
import { FormResponsesDocument } from '@/graphql/Crm';
import { useQuery } from 'urql';
import { TitleBar } from '@/ui/TitleBar';
import { Layout } from '@/components/layout/Layout';
import { buttonCls } from '@/ui/style';
import { cn } from '@/ui/cn';
import * as ToggleGroupPrimitive from '@radix-ui/react-toggle-group';

const Page = () => {
  const [{ data }] = useQuery({query: FormResponsesDocument});

  const dataset = React.useMemo(() => data?.formResponses?.nodes || [], [data?.formResponses?.nodes]);

  const types = React.useMemo(() => {
    const types = new Set<string>();
    dataset.forEach(x => types.add(x.type));
    return [...types.values()].sort();
  }, [dataset]);

  const [state, setState] = React.useState('');
  const currentData = React.useMemo(() => {
    const selected = state || types[0];
    return dataset.filter(x => x.type === selected);
  }, [types, state, dataset]);

  const columns = React.useMemo(() => {
    const columns = {} as { [key: string]: string };
    currentData.forEach(x => Object.keys(x.data).forEach(x => {
      columns[x] = '';
    }))
    return Object.keys(columns);
  }, [currentData]);

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
    },
    [data],
  );

  return (
    <Layout requireAdmin>
      <div className="container col-feature">
        <TitleBar title="Odeslané formuláře">
          <button type="button" className={buttonCls({ variant: 'outline' })} onClick={saveData}>
            Export všech
          </button>
        </TitleBar>

        <ToggleGroupPrimitive.Root value={state || types[0]} onValueChange={setState} type="single" className="grow">
          {types.map(type => (
            <ToggleGroupPrimitive.Item
              key={`group-item-${type}`}
              value={type}
              className={cn(
                'group data-[state=on]:bg-accent-5 bg-accent-3 text-accent-11',
                'border-y px-2 py-1 text-sm first:rounded-l-xl first:border-x last:rounded-r-xl last:border-x',
                'border-accent-5 data-[state=on]:border-accent-8',
                'focus:relative focus:outline-none focus-visible:z-30 focus-visible:ring focus-visible:ring-accent-8',
              )}
            >
              {type}
            </ToggleGroupPrimitive.Item>
          ))}
        </ToggleGroupPrimitive.Root>

        <table className="w-full text-sm text-left text-neutral-12 mt-3">
          <thead>
            <tr>
              {columns.map(x => <th key={x}>{x}</th>)}
              <th className="text-right">Vytvořeno</th>
            </tr>
          </thead>
          <tbody>
            {data?.formResponses?.nodes.filter(x => state ? x.type === state : x.type === types[0])?.map((row, i) => (
              <tr key={i} className="even:bg-neutral-2 odd:bg-neutral-1 border-b">
                {columns.map(x => (
                  <td key={x}>
                    {x === 'date' ? fullDateFormatter.format(new Date(row.data[x] as string)) : row.data[x]}
                  </td>
                ))}
                {/* <td className="py-1">
                  {row.data.name} {row.data.surname}
                </td>
                <td>{row.data.email}</td>
                <td>{row.data.phone}</td>
                <td>{row.data.yearofbirth}</td>
                */}
                <td className="text-right">
                  {row.createdAt ? fullDateFormatter.format(new Date(row.createdAt)) : ''}
                </td>
              </tr>
            ))}
          </tbody>
        </table>
      </div>
    </Layout>
  );
}


export default Page;
