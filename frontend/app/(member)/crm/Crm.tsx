'use client';

import * as React from 'react';
import { fullDateFormatter } from '@/ui/format';
import { FormResponsesDocument } from '@/graphql/Crm';
import { useClient, useQuery } from 'urql';
import { PageHeader } from '@/ui/PageHeader';
import { Table } from '@/ui/Table';
import { RadioButtonGroup } from '@/ui/fields/RadioButtonGroupElement';
import { exportFormResponses } from '@/ui/reports/export-form-responses';

export function Crm() {
  const client = useClient();
  const [{ data, fetching }] = useQuery({ query: FormResponsesDocument });

  const dataset = React.useMemo(() => {
    return (data?.formResponses?.nodes || []).map((x) => ({
      ...x,
      data: typeof x.data === 'string' ? JSON.parse(x.data) : x.data,
    }));
  }, [data?.formResponses?.nodes]);

  const types = React.useMemo(() => {
    const types = new Set<string>();
    for (const entry of dataset) {
      if (entry.type !== 'Zpětná vazba, web 05/2023') types.add(entry.type);
    }
    return [...types.values()].toSorted();
  }, [dataset]);

  const [state, setState] = React.useState('');
  const currentData = React.useMemo(() => {
    return dataset.filter((x) => x.type === (state || types[0]));
  }, [types, state, dataset]);

  const columns = React.useMemo(() => {
    const columns = new Set<string>();
    for (const entry of currentData) {
      for (const column in entry.data) {
        columns.add(column);
      }
    }
    return [...columns.values()].toSorted();
  }, [currentData]);

  return (
    <div className="container col-feature">
      <PageHeader
        title="Odeslané formuláře"
        actions={[
          {
            id: 'crm.export',
            key: 'crm.export',
            label: 'Export všech',
            group: 'primary',
            execute: () => exportFormResponses(client),
          },
        ]}
      />

      <RadioButtonGroup
        className="grow"
        value={state || types[0] || ''}
        options={types.map((type) => ({ id: type, label: type }))}
        onValueChange={setState}
      />

      <Table
        className="mt-3 text-neutral-12"
        busy={fetching}
        columns={[
          ...columns,
          <span key="createdAt" className="block text-right">Vytvořeno</span>,
        ]}
      >
        {currentData.map((row, i) => (
          <tr key={i} className="even:bg-neutral-2 odd:bg-neutral-1 border-b">
            {columns.map((x) => (
              <td key={x}>
                {x === 'date'
                  ? fullDateFormatter.format(new Date(row.data[x] as string))
                  : x === 'email' && row.data[x]
                    ? <a className="underline" href={`mailto:${row.data[x]}`}>{row.data[x]}</a>
                    : x === 'phone' && row.data[x]
                      ? <a className="underline" href={`tel:${String(row.data[x]).replaceAll(' ', '')}`}>{row.data[x]}</a>
                      : row.data[x]}
              </td>
            ))}
            <td className="text-right">
              {row.createdAt ? fullDateFormatter.format(new Date(row.createdAt)) : ''}
            </td>
          </tr>
        ))}
      </Table>
    </div>
  );
}
