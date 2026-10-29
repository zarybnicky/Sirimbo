'use client';

import { PageHeader } from '@/ui/PageHeader';
import React from 'react';
import {
  createParser,
  parseAsArrayOf,
  parseAsString,
  parseAsStringEnum,
  useQueryState,
  useQueryStates,
} from 'nuqs';
import { Tab, TabMenu } from '@/ui/TabMenu';
import { useClient, useQuery } from 'urql';
import { TransactionAccountsDocument, TransactionsDocument } from '@/graphql/Payment';
import type { TransactionSource } from '@/graphql';
import type { AccountFragment } from '@/graphql/Payment';
import { PersonAccountsDocument, UnpaidPaymentsDocument } from '@/graphql/Person';
import {
  describePosting,
  moneyFormatter,
  numericDateWithYearFormatter,
} from '@/ui/format';
import { buttonCls } from '@/ui/style';
import { exportBalanceSheet } from '@/ui/reports/export-balance-sheet';
import Link from 'next/link';
import { keyIsNonNull } from '@/lib/truthyFilter';
import { useActionMap } from '@/lib/actions';
import { paymentActions } from '@/lib/actions/payment';
import { ActionGroup } from '@/ui/ActionGroup';
import { usePaginatedQuery } from '@/lib/usePaginatedQuery';
import { Pagination } from '@/ui/Pagination';
import { Table } from '@/ui/Table';
import { FacetFilter } from '@/ui/FacetFilter';
import { FacetRadioGroup } from '@/ui/FacetRadioGroup';
import { DefinitionList } from '@/ui/DefinitionList';
import { TextField } from '@/ui/fields/text';
import { parseDatePickerValue } from '@/ui/fields/date-utils';
import { compareText } from '@/lib/compare';
import { cn } from '@/lib/cn';
import { Popover, PopoverContent, PopoverTrigger } from '@/ui/popover';
import { ChevronDown, SlidersHorizontal } from 'lucide-react';
import { add } from 'date-arithmetic';

const TRANSACTIONS_PAGE_SIZE = 50;
const sourceOptions: { value: TransactionSource; label: string }[] = [
  { value: 'AUTO_CREDIT', label: 'Úhrady z kreditu' },
  { value: 'MANUAL_CREDIT', label: 'Ruční změny kreditu' },
];
const defaultSources = sourceOptions.map((option) => option.value);

const transactionDateParser = createParser({
  parse: (value) => {
    const date = parseDatePickerValue(value, 'date');
    return typeof date === 'string' ? date : null;
  },
  serialize: (value) => value,
});

export function Payments() {
  const [tab, setTab] = useQueryState(
    'tab',
    parseAsString.withOptions({ history: 'push' }),
  );

  return (
    <>
      <PageHeader title="Platby" />
      <TabMenu selected={tab} onSelect={setTab}>
        <Tab id="info" title="Stav kreditu">
          <AccountOverview />
        </Tab>
        <Tab id="unpaid" title="Nezaplacené">
          <UnpaidPayments />
        </Tab>
        <Tab id="transactions" title="Transakce">
          <Transactions />
        </Tab>
      </TabMenu>
    </>
  );
}

const AccountOverview = React.memo(function AccountOverview() {
  const client = useClient();
  const [{ data, fetching, error }] = useQuery({ query: PersonAccountsDocument });

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
        columns={[
          'Osoba',
          <span key="balance" className="block text-right">
            Zůstatek
          </span>,
        ]}
        busy={fetching}
        error={error}
      >
        {(data?.people?.nodes ?? []).map((person) => (
          <tr key={person.id} className="border-t border-neutral-4 align-top">
            <th scope="row">{person.name}</th>
            <td className="whitespace-nowrap text-right">
              {person.accountsList.map((account) => (
                <div key={account.id}>
                  {moneyFormatter.format({
                    amount: account.balance ?? '0',
                    currency: account.currency,
                  })}
                </div>
              ))}
            </td>
          </tr>
        ))}
      </Table>
    </>
  );
});

const UnpaidPayments = React.memo(function UnpaidPayments() {
  const [{ data, fetching, error }] = useQuery({ query: UnpaidPaymentsDocument });
  const unpaid = data?.unpaidPayments?.filter(keyIsNonNull('payment')) ?? [];
  const actionMap = useActionMap(
    paymentActions,
    unpaid.map((x) => x.payment),
  );

  return (
    <Table
      columns={[
        <span key="actions" className="sr-only">
          Akce
        </span>,
        'Osoba',
        'Popis',
        <span key="amount" className="block text-right">
          Částka
        </span>,
      ]}
      busy={fetching}
      error={error}
      empty="Žádné nezaplacené platby."
    >
      {unpaid.map((debtor) => (
        <tr key={debtor.id} className="border-t border-neutral-4 align-top">
          <td className="w-0">
            <ActionGroup variant="row" actions={actionMap.get(debtor.payment.id)!} />
          </td>
          <th scope="row">{debtor.person?.name}</th>
          <td>{describePosting(debtor.payment)}</td>
          <td className="whitespace-nowrap text-right">
            {moneyFormatter.format(debtor.price)}
          </td>
        </tr>
      ))}
    </Table>
  );
});

function accountLabel(account: AccountFragment) {
  return `${account.personId === null ? 'Klub' : (account.person?.name ?? '—')} (${account.currency})`;
}

function Transactions() {
  const [filters, setFilters] = useQueryStates(
    {
      accountIds: parseAsArrayOf(parseAsString).withDefault([]),
      sources: parseAsArrayOf(
        parseAsStringEnum<TransactionSource>(defaultSources),
      ).withDefault(defaultSources),
      since: transactionDateParser,
      until: transactionDateParser,
      search: parseAsString.withDefault(''),
    },
    { shallow: true, scroll: false },
  );
  const search = React.useDeferredValue(filters.search.trim());
  const searchPending = search !== filters.search.trim();
  const activeFacetCount =
    Number(filters.accountIds.length > 0) +
    Number(filters.sources.length !== defaultSources.length) +
    Number(!!(filters.since || filters.until));
  const [showFilters, setShowFilters] = React.useState(true);
  const [accounts] = useQuery({ query: TransactionAccountsDocument });
  const accountIds = filters.accountIds.length > 0 ? filters.accountIds : null;
  const { data, fetching, error, pagination } = usePaginatedQuery({
    query: TransactionsDocument,
    variables: (offset) => ({
      first: TRANSACTIONS_PAGE_SIZE,
      offset,
      accountIds,
      condition: {
        accountIds,
        sources:
          filters.sources.length === defaultSources.length ? null : filters.sources,
        date: {
          since: filters.since
            ? new Date(`${filters.since}T00:00:00`).toISOString()
            : null,
          until: filters.until
            ? add(new Date(`${filters.until}T00:00:00`), 1, 'day').toISOString()
            : null,
        },
        search,
      },
    }),
    getTotal: (data) => data.transactions?.totalCount,
    limit: TRANSACTIONS_PAGE_SIZE,
    pageKey: 'page',
    pause: searchPending,
  });
  const { total, page, limit } = pagination;
  const isPending = fetching || searchPending;
  const queryError = error ?? accounts.error;
  const change = (value: Partial<typeof filters>) => {
    void setFilters(value);
    void pagination.onPageChange(1);
  };
  const views: ({ value: string; label: string } & Pick<
    typeof filters,
    'accountIds' | 'sources'
  >)[] = [
    { value: 'all', label: 'Vše', accountIds: [], sources: defaultSources },
    {
      value: 'turnover',
      label: 'Obrat klubu',
      sources: defaultSources,
      accountIds: (accounts.data?.accountsList ?? [])
        .filter((x) => x.personId === null)
        .map((x) => x.id),
    },
    {
      value: 'deposits',
      label: 'Dobití kreditu',
      accountIds: [],
      sources: ['MANUAL_CREDIT'],
    },
  ];

  return (
    <>
      <div className="flex flex-wrap items-center gap-2 py-2">
        {!queryError && (
          <p
            role="status"
            aria-busy={isPending}
            className="whitespace-nowrap text-xs tabular-nums text-neutral-11"
          >
            {total === 0
              ? '0 / 0'
              : `${(page - 1) * limit + 1}–${Math.min(page * limit, total)} / ${total}`}
          </p>
        )}
        <button
          type="button"
          aria-expanded={showFilters}
          className={buttonCls({ display: 'compact', variant: 'facet', size: 'sm' })}
          onClick={() => setShowFilters((visible) => !visible)}
        >
          <SlidersHorizontal className="size-3" aria-hidden />
          Filtry{activeFacetCount > 0 && ` (${activeFacetCount})`}
          <ChevronDown
            className={cn('size-3', showFilters && 'rotate-180')}
            aria-hidden
          />
        </button>

        <Popover>
          <PopoverTrigger
            className={buttonCls({ display: 'compact', variant: 'facet', size: 'sm' })}
            aria-pressed={!!(filters.since || filters.until)}
          >
            Období <ChevronDown />
          </PopoverTrigger>
          <PopoverContent>
            <div className="grid gap-2 pr-6 sm:grid-cols-2">
              <TextField
                name="since"
                type="date"
                label="Datum od"
                value={filters.since ?? ''}
                max={filters.until ?? undefined}
                onChange={(event) =>
                  change({
                    since: transactionDateParser.parse(event.currentTarget.value),
                  })
                }
              />
              <TextField
                name="until"
                type="date"
                label="Datum do"
                value={filters.until ?? ''}
                min={filters.since ?? undefined}
                onChange={(event) =>
                  change({
                    until: transactionDateParser.parse(event.currentTarget.value),
                  })
                }
              />
            </div>
          </PopoverContent>
        </Popover>

        {(activeFacetCount > 0 || filters.search) && (
          <button
            type="button"
            className="px-2 py-1.5 text-xs text-accent-11 underline hover:text-accent-12"
            onClick={() =>
              change({
                accountIds: [],
                sources: defaultSources,
                since: null,
                until: null,
                search: '',
              })
            }
          >
            Zrušit filtry
          </button>
        )}
        <TextField
          name="search"
          type="search"
          aria-label="Vyhledat transakce"
          placeholder="Popis, VS nebo SS…"
          className="min-w-40 flex-1 sm:max-w-64 ml-auto"
          inputClassName="border-accent-6 bg-accent-2 py-1.5 text-sm text-neutral-12 placeholder:text-neutral-10 focus:border-accent-8 focus:ring-accent-8"
          value={filters.search}
          onChange={(event) => change({ search: event.currentTarget.value })}
        />
      </div>

      <div
        className={cn(
          'mb-3 w-full grid-cols-1 gap-x-3 gap-y-2 sm:grid-cols-[max-content_minmax(0,1fr)]',
          showFilters ? 'grid' : 'hidden',
        )}
      >
        <FacetRadioGroup
          inline
          name="transaction-view"
          label="Pohled"
          value={
            views.find(
              (view) =>
                view.accountIds.length === filters.accountIds.length &&
                view.accountIds.every((id) => filters.accountIds.includes(id)) &&
                view.sources.length === filters.sources.length &&
                view.sources.every((x) => filters.sources.includes(x)),
            )?.value ?? ''
          }
          options={views.filter(
            (view) => view.value !== 'turnover' || view.accountIds.length > 0,
          )}
          onChange={(value) => {
            const view = views.find((view) => view.value === value)!;
            change({ accountIds: view.accountIds, sources: view.sources });
          }}
        />
        <FacetFilter
          inline
          label="Zdroj"
          value={filters.sources}
          defaultValue={defaultSources}
          options={sourceOptions}
          onChange={(sources) => change({ sources })}
        />
        <FacetFilter
          inline
          label="Účty"
          value={filters.accountIds}
          maxVisibleOptions={5}
          moreLabel="Další"
          options={(accounts.data?.accountsList ?? [])
            .toSorted(
              (a, b) =>
                Number(a.personId !== null) - Number(b.personId !== null) ||
                compareText(accountLabel(a), accountLabel(b)),
            )
            .map((x) => ({ value: x.id, label: accountLabel(x) }))}
          onChange={(accountIds) => change({ accountIds })}
        />
      </div>

      <Table
        columns={[
          'Datum',
          'Účet',
          'Popis',
          <span key="amount" className="block text-right">
            Částka
          </span>,
        ]}
        caption={
          filters.accountIds.length > 0 && (
            <DefinitionList
              className="text-sm tabular font-normal"
              items={(accounts.data?.accountsList ?? [])
                .filter(({ id }) => filters.accountIds.includes(id))
                .map(
                  (x) =>
                    [
                      accountLabel(x),
                      moneyFormatter.format({
                        amount: x.balance ?? '0',
                        currency: x.currency,
                      }),
                    ] as const,
                )}
            />
          )
        }
        busy={isPending}
        error={queryError}
        empty="Žádné pohyby."
      >
        {(data?.transactions?.nodes ?? []).flatMap((transaction) =>
          transaction.postingsList.map((posting) => {
            const description =
              transaction.description || describePosting(transaction.payment, 'neutral');
            return (
              <tr key={posting.id} className="border-t border-neutral-4 align-top">
                <td className="whitespace-nowrap text-neutral-11">
                  {numericDateWithYearFormatter.format(
                    new Date(transaction.effectiveDate),
                  )}
                </td>
                <th scope="row">
                  {posting.account ? accountLabel(posting.account) : '—'}
                </th>
                <td>
                  {transaction.payment ? (
                    <Link
                      href={`/platby/${transaction.payment.id}`}
                      className="text-accent-11 underline"
                    >
                      {description}
                    </Link>
                  ) : (
                    description
                  )}
                  {(transaction.payment?.variableSymbol ||
                    transaction.payment?.specificSymbol) && (
                    <div className="inline-flex gap-1 pl-1 text-xs text-neutral-11">
                      {transaction.payment.variableSymbol && (
                        <div>VS: {transaction.payment.variableSymbol}</div>
                      )}
                      {transaction.payment.specificSymbol && (
                        <div>SS: {transaction.payment.specificSymbol}</div>
                      )}
                    </div>
                  )}
                </td>
                <td className="whitespace-nowrap text-right">
                  {posting.account
                    ? moneyFormatter.format({
                        amount: posting.amount,
                        currency: posting.account.currency,
                      })
                    : posting.amount}
                </td>
              </tr>
            );
          }),
        )}
      </Table>
      {!queryError && <Pagination {...pagination} />}
    </>
  );
}
