import React from 'react';
import { useQuery } from 'urql';
import { PersonAccountsDocument, PersonAccountsQuery } from '@/graphql/Person';
import { moneyFormatter } from '@/ui/format';
import { buttonCls } from '@/ui/style';
import Link from 'next/link';
import { Table } from '@/ui/Table';
import { compareText } from '@/lib/compare';
import { cn } from '@/lib/cn';
import { ArrowDown, ArrowUpDown, Download } from 'lucide-react';
import { useAsyncCallback } from 'react-async-hook';
import { Spinner } from '@/ui/Spinner';
import { saveAs } from 'file-saver';

export const AccountList = React.memo(function AccountList() {
  const [{ data, fetching, error }] = useQuery({ query: PersonAccountsDocument });
  const [sort, setSort] = React.useState({ column: 'name', descending: false });
  const people = data?.people?.nodes ?? [];
  const currencies = [
    ...new Set(people.flatMap((p) => p.accountsList.map((x) => x.currency))),
  ].toSorted(compareText);
  const columns = [
    { id: 'name', label: 'Osoba' },
    ...currencies.map((currency) => ({
      id: currency,
      label: currencies.length === 1 ? 'Zůstatek' : `Zůstatek (${currency})`,
    })),
  ];
  const rows = people.toSorted((a, b) => {
    const nameOrder =
      compareText(a.lastName, b.lastName) || compareText(a.firstName, b.firstName);
    if (sort.column === 'name') return sort.descending ? -nameOrder : nameOrder;

    const left = a.accountsList.find((x) => x.currency === sort.column);
    const right = b.accountsList.find((x) => x.currency === sort.column);
    if (!left || !right) return left ? -1 : right ? 1 : nameOrder;
    const balanceOrder = Number(left.balance ?? '0') - Number(right.balance ?? '0');
    return (sort.descending ? -balanceOrder : balanceOrder) || nameOrder;
  });
  const exportCurrentState = useAsyncCallback(() => exportBalanceSheet(columns, rows));

  return (
    <Table
      actions={
        <button
          type="button"
          className={buttonCls({ display: 'compact', variant: 'outline', size: 'sm' })}
          disabled={fetching || rows.length === 0 || exportCurrentState.loading}
          onClick={() => exportCurrentState.execute()}
        >
          {exportCurrentState.loading ? <Spinner /> : <Download aria-hidden />}
          Export
        </button>
      }
      columns={columns.map(({ id, label }) => (
        <div key={id} className={id === 'name' ? undefined : 'text-right'}>
          <button
            type="button"
            aria-pressed={sort.column === id}
            className={buttonCls({
              display: 'none',
              variant: 'none',
              size: 'none',
              className:
                'inline-flex items-center gap-1 whitespace-nowrap text-accent-11',
            })}
            onClick={() =>
              setSort((x) => ({
                column: id,
                descending: x.column === id ? !x.descending : false,
              }))
            }
          >
            {label}
            {sort.column === id ? (
              <>
                <ArrowDown
                  className={cn('size-4', !sort.descending && 'rotate-180')}
                  aria-hidden
                />
                <span className="sr-only">
                  {sort.descending ? ', sestupně' : ', vzestupně'}
                </span>
              </>
            ) : (
              <ArrowUpDown className="size-4" aria-hidden />
            )}
          </button>
        </div>
      ))}
      busy={fetching}
      error={error}
    >
      {rows.map((person) => (
        <tr key={person.id} className="border-t border-neutral-4 align-top">
          <th scope="row">
            <Link href={`/clenove/${person.id}`} className="text-accent-11 underline">
              {person.name}
            </Link>
          </th>
          {currencies.map((currency) => {
            const account = person.accountsList.find((x) => x.currency === currency);
            return (
              <td key={currency} className="whitespace-nowrap text-right">
                {account
                  ? moneyFormatter.format({ amount: account.balance ?? '0', currency })
                  : ''}
              </td>
            );
          })}
        </tr>
      ))}
    </Table>
  );
});

type Person = NonNullable<PersonAccountsQuery['people']>['nodes'][number];

async function exportBalanceSheet(
  columns: readonly { id: string; label: string }[],
  people: readonly Person[],
) {
  const { Workbook } = await import('exceljs');
  const workbook = new Workbook();
  const worksheet = workbook.addWorksheet('Stav kreditu');
  worksheet.columns = columns.map(({ id, label }) => ({
    header: label,
    key: id,
    width: Math.max(label.length + 4, id === 'name' ? 32 : 20),
    ...(id === 'name' ? {} : { style: { numFmt: `#,##0.00 "${id}"` } }),
  }));
  worksheet.getRow(1).font = { bold: true };
  worksheet.addRows(
    people.map((person) =>
      columns.map(({ id }) => {
        if (id === 'name') return person.name;
        const account = person.accountsList.find((x) => x.currency === id);
        return account ? Number(account.balance ?? '0') : null;
      }),
    ),
  );

  const buf = await workbook.xlsx.writeBuffer();
  saveAs(new Blob([buf]), 'Stav kreditu.xlsx');
}
