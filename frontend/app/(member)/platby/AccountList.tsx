import React from 'react';
import { useClient, useQuery } from 'urql';
import { PersonAccountsDocument } from '@/graphql/Person';
import { moneyFormatter } from '@/ui/format';
import { buttonCls } from '@/ui/style';
import { exportBalanceSheet } from '@/ui/reports/export-balance-sheet';
import Link from 'next/link';
import { Table } from '@/ui/Table';
import { compareText } from '@/lib/compare';
import { cn } from '@/lib/cn';
import { ArrowDown, ArrowUpDown } from 'lucide-react';

export const AccountList = React.memo(function AccountList() {
  const client = useClient();
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

  return (
    <>
      <button
        type="button"
        className={buttonCls({ className: 'mb-3' })}
        onClick={() => exportBalanceSheet(client)}
      >
        Přehled plateb 2023
      </button>
      <Table
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
    </>
  );
});
