import { TitleBar } from '@/ui/TitleBar';
import { Layout } from '@/components/layout/Layout';
import React from 'react';
import { StringParam, useQueryParam } from 'use-query-params';
import { TabMenu } from '@/ui/TabMenu';
import { useClient, useMutation, useQuery } from 'urql';
import {
  TenantManualCreditTransactionsDocument,
  TenantTurnoverPageDocument,
  type TenantManualCreditTransactionsQuery,
  type TenantTurnoverPageQuery,
} from '@/graphql/Payment';
import { PersonAccountsDocument, UnpaidPaymentsDocument } from '@/graphql/Person';
import { describePosting, moneyFormatter, numericDateFormatter } from '@/ui/format';
import { DropdownMenu, DropdownMenuButton, DropdownMenuContent, DropdownMenuTrigger } from '@/ui/dropdown';
import { MarkAsPaidDocument } from '@/graphql/Payment';
import { buttonCls } from '@/ui/style';
import { exportBalanceSheet } from '@/ui/reports/export-balance-sheet';
import { Spinner } from '@/ui/Spinner';

type TenantAccountPage = NonNullable<TenantTurnoverPageQuery['accountsList']>[number];
type TenantPosting = TenantAccountPage['postingsList'][number];
type TenantAccount = Omit<TenantAccountPage, 'postingsList'> & { postings: TenantPosting[] };
type TenantTransaction = NonNullable<TenantPosting['transaction']>;
type ManualCreditTransaction = NonNullable<
  NonNullable<
    NonNullable<TenantManualCreditTransactionsQuery['tenant']>['transactions']
  >['nodes']
>[number];

const TURNOVER_PAGE_SIZE = 50;
const DEPOSIT_PAGE_SIZE = 50;

export default function PaymentsPage() {
  const [tab, setTab] = useQueryParam('tab', StringParam);

  const tabs = [
    {
      id: 'info',
      title: <>Stav kreditu</>,
      contents: () => <AccountOverview key="info" />,
    },
    {
      id: 'unpaid',
      title: <>Nezaplacené</>,
      contents: () => <UnpaidPayments key="unpaid" />,
    },
    {
      id: 'turnover',
      title: <>Obrat klubu</>,
      contents: () => <TenantTurnover key="turnover" />,
    },
    {
      id: 'deposits',
      title: <>Dobití kreditu</>,
      contents: () => <TenantDeposits key="deposits" />,
    },
  ];

  return (
    <Layout requireAdmin>
      <TitleBar title="Platby" />
      <TabMenu selected={tab} onSelect={setTab} options={tabs} />
    </Layout>
  );
};

function AccountOverview() {
  const [{ data }] = useQuery({ query: PersonAccountsDocument });

  return <>
    <button type="button" className={buttonCls()} onClick={exportBalanceSheet}>
      Přehled plateb 2023
    </button>
    {data?.filteredPeopleList?.map(x => (
      <div key={x.id} className="flex flex-wrap gap-2 justify-between even:bg-neutral-2 odd:bg-neutral-1 border-b">
        <span>{x.name}</span>
        <span>
          {moneyFormatter.format({ amount: x.accountsList.find(Boolean)?.balance ?? '0', currency: 'CZK' })}
        </span>
      </div>
    ))}
  </>;
};

function UnpaidPayments() {
  const [{ data }] = useQuery({ query: UnpaidPaymentsDocument });
  const { unpaidPayments } = data || {};
  const markAsPaid = useMutation(MarkAsPaidDocument)[1];

  return (
    <>
      {unpaidPayments?.map((x) => (
        <div
          key={x.id}
          className="flex flex-wrap gap-4 justify-between even:bg-neutral-2 odd:bg-neutral-1 border-b"
        >
          <span className="grow">{x.person?.name}</span>
          <span>{describePosting(x.payment!)}</span>
          <span>{moneyFormatter.format(x.price)}</span>
          <span>
            <DropdownMenu>
              <DropdownMenuTrigger.CornerDots className="relative top-0 right-0" />
              <DropdownMenuContent align="end">
                <DropdownMenuButton onClick={() => markAsPaid({ id: x.payment?.id! })}>
                  Označit jako zaplacenou
                </DropdownMenuButton>
              </DropdownMenuContent>
            </DropdownMenu>
          </span>
        </div>
      ))}
    </>
  );
}

function TenantTurnover() {
  const client = useClient();
  const [accounts, setAccounts] = React.useState<Map<string, TenantAccount>>(new Map());
  const [accountOrder, setAccountOrder] = React.useState<string[]>([]);
  const [nextOffset, setNextOffset] = React.useState(0);
  const [hasMore, setHasMore] = React.useState(false);
  const [loading, setLoading] = React.useState(false);
  const [initialized, setInitialized] = React.useState(false);
  const [error, setError] = React.useState<string | null>(null);

  const fetchPage = React.useCallback(
    async (offset: number) => {
      setLoading(true);
      const result = await client
        .query(TenantTurnoverPageDocument, { offset, postingsLimit: TURNOVER_PAGE_SIZE + 1 })
        .toPromise();

      if (result.error) {
        setError(result.error.message);
        setLoading(false);
        return;
      }

      const pageAccounts = result.data?.accountsList ?? [];

      setAccounts((prev) => {
        const map = new Map(prev);
        for (const account of pageAccounts) {
          const { postingsList, ...accountInfo } = account;
          const existing = map.get(accountInfo.id);
          const mergedPostings = existing ? [...existing.postings] : [];
          const nextPostings = postingsList.slice(0, TURNOVER_PAGE_SIZE);

          for (const posting of nextPostings) {
            if (!mergedPostings.some((item) => item.id === posting.id)) {
              mergedPostings.push(posting);
            }
          }

          map.set(accountInfo.id, {
            ...accountInfo,
            postings: mergedPostings,
          });
        }

        return map;
      });

      setAccountOrder((prev) => {
        const order = [...prev];
        const seen = new Set(order);
        for (const account of pageAccounts) {
          if (!seen.has(account.id)) {
            order.push(account.id);
            seen.add(account.id);
          }
        }
        return order;
      });

      const more = pageAccounts.some((account) => account.postingsList.length > TURNOVER_PAGE_SIZE);
      setHasMore(more);
      setNextOffset(offset + TURNOVER_PAGE_SIZE);
      setInitialized(true);
      setError(null);
      setLoading(false);
    },
    [client],
  );

  React.useEffect(() => {
    void fetchPage(0);
  }, [fetchPage]);

  const orderedAccounts = React.useMemo(
    () =>
      accountOrder
        .map((id) => accounts.get(id))
        .filter((account): account is TenantAccount => Boolean(account)),
    [accountOrder, accounts],
  );
  const isEmpty = orderedAccounts.every((account) => account.postings.length === 0);

  if (!initialized && loading) {
    return (
      <div className="flex justify-center py-6">
        <Spinner />
      </div>
    );
  }

  if (error) {
    return <p className="text-sm text-red-600">Nepodařilo se načíst obrat: {error}</p>;
  }

  if (initialized && isEmpty) {
    return <p>Žádné pohyby.</p>;
  }

  return (
    <div className="flex flex-col gap-6">
      {orderedAccounts.map((account) => {
        const postings = [...account.postings]
          .filter((posting): posting is TenantPosting & { transaction: TenantTransaction } => Boolean(posting.transaction))
          .sort((a, b) => (b.transaction.effectiveDate || '').localeCompare(a.transaction.effectiveDate || ''));

        if (postings.length === 0) {
          return (
            <section key={account.id} className="space-y-2">
              <header className="flex flex-wrap items-baseline justify-between gap-2">
                <h3 className="text-lg font-semibold">Účet {account.currency}</h3>
                <span className="text-sm text-neutral-500">
                  Zůstatek: {moneyFormatter.format({ amount: account.balance ?? '0', currency: account.currency })}
                </span>
              </header>
              <p className="text-sm text-neutral-500">Žádné zaúčtované pohyby.</p>
            </section>
          );
        }

        return (
          <section key={account.id} className="space-y-2">
            <header className="flex flex-wrap items-baseline justify-between gap-2">
              <h3 className="text-lg font-semibold">Účet {account.currency}</h3>
              <span className="text-sm text-neutral-500">
                Zůstatek: {moneyFormatter.format({ amount: account.balance ?? '0', currency: account.currency })}
              </span>
            </header>

            <div className="overflow-hidden rounded-md border divide-y">
              {postings.map((posting) => {
                const transaction = posting.transaction;
                const payment = transaction.payment;
                const displayDate = transaction.effectiveDate
                  ? numericDateFormatter.format(new Date(transaction.effectiveDate))
                  : '';
                const description = transaction.description || describePosting(payment, posting);
                const variableSymbol = payment?.variableSymbol;
                const specificSymbol = payment?.specificSymbol;

                return (
                  <div
                    key={posting.id}
                    className="flex flex-wrap items-start justify-between gap-3 bg-neutral-1 px-3 py-2 odd:bg-neutral-1 even:bg-neutral-2"
                  >
                    <span className="text-sm text-neutral-600 min-w-[5rem]">{displayDate}</span>
                    <div className="flex min-w-[12rem] flex-1 flex-col gap-1">
                      <span>{description}</span>
                      {(variableSymbol || specificSymbol) && (
                        <span className="text-xs text-neutral-500">
                          {variableSymbol && <>VS: {variableSymbol}</>}
                          {variableSymbol && specificSymbol && ' • '}
                          {specificSymbol && <>SS: {specificSymbol}</>}
                        </span>
                      )}
                    </div>
                    <span className="font-medium">
                      {moneyFormatter.format({ amount: posting.amount ?? '0', currency: account.currency })}
                    </span>
                  </div>
                );
              })}
            </div>
          </section>
        );
      })}

      {hasMore && (
        <div className="flex justify-center">
          <button
            type="button"
            className={buttonCls({ variant: 'outline' })}
            onClick={() => {
              if (!loading) {
                void fetchPage(nextOffset);
              }
            }}
            disabled={loading}
          >
            {loading ? 'Načítání…' : 'Načíst starší...'}
          </button>
        </div>
      )}
    </div>
  );
}

interface TenantDepositsPageProps {
  cursor?: string | null;
  onLoadMore?: (cursor: string) => void;
}

function TenantDeposits() {
  const [cursors, setCursors] = React.useState<Array<string | null>>([null]);

  const handleLoadMore = React.useCallback((cursor: string) => {
    setCursors((prev) => [...prev, cursor]);
  }, []);

  return (
    <div className="space-y-4">
      <div className="overflow-hidden rounded-md border divide-y">
        {cursors.map((cursor, index) => (
          <TenantDepositsPage
            key={`${cursor ?? 'start'}-${index}`}
            cursor={cursor}
            onLoadMore={index === cursors.length - 1 ? handleLoadMore : undefined}
          />
        ))}
      </div>
    </div>
  );
}

function TenantDepositsPage({ cursor, onLoadMore }: TenantDepositsPageProps) {
  const [{ data, fetching, error }] = useQuery({
    query: TenantManualCreditTransactionsDocument,
    variables: { first: DEPOSIT_PAGE_SIZE, cursor: cursor ?? undefined },
  });

  const transactions = data?.tenant?.transactions;
  const nodes = transactions?.nodes ?? [];
  const hasMore = transactions?.pageInfo.hasNextPage ?? false;
  const endCursor = transactions?.pageInfo.endCursor ?? undefined;

  if (!data && fetching) {
    return (
      <div className="flex justify-center py-6">
        <Spinner />
      </div>
    );
  }

  if (error) {
    return (
      <div className="px-3 py-2 text-sm text-red-600">
        {cursor ? 'Nepodařilo se načíst další vklady.' : `Nepodařilo se načíst vklady: ${error.message}`}
      </div>
    );
  }

  if (!fetching && nodes.length === 0 && !cursor) {
    return <div className="px-3 py-2 text-sm text-neutral-500">Žádná ruční dobití.</div>;
  }

  return (
    <>
      {nodes.map((transaction) => (
        <DepositRow key={transaction.id} transaction={transaction} />
      ))}
      {hasMore && onLoadMore && endCursor && (
        <div className="flex justify-center py-3">
          <button
            type="button"
            className={buttonCls({ variant: 'outline' })}
            onClick={() => onLoadMore(endCursor)}
            disabled={fetching}
          >
            {fetching ? 'Načítání…' : 'Načíst starší...'}
          </button>
        </div>
      )}
    </>
  );
}

function DepositRow({ transaction }: { transaction: ManualCreditTransaction }) {
  const effectiveDate = transaction.effectiveDate
    ? numericDateFormatter.format(new Date(transaction.effectiveDate))
    : '';

  const personPosting = transaction.postingsList.find((posting) => posting.account?.person);
  const accountPosting = personPosting ?? transaction.postingsList[0];
  const personName = personPosting?.account?.person?.name ?? '—';
  const amount = accountPosting?.amount ?? '0';
  const currency = accountPosting?.account?.currency ?? 'CZK';
  const variableSymbol = transaction.payment?.variableSymbol;
  const specificSymbol = transaction.payment?.specificSymbol;

  return (
    <div className="flex flex-wrap items-start justify-between gap-3 bg-neutral-1 px-3 py-2 odd:bg-neutral-1 even:bg-neutral-2">
      <span className="text-sm text-neutral-600 min-w-[5rem]">{effectiveDate}</span>
      <div className="flex min-w-[12rem] flex-1 flex-col gap-1">
        <span className="font-medium">{personName}</span>
        <span className="text-sm text-neutral-600">{transaction.description || 'Dobití kreditu'}</span>
        {(variableSymbol || specificSymbol) && (
          <span className="text-xs text-neutral-500">
            {variableSymbol && <>VS: {variableSymbol}</>}
            {variableSymbol && specificSymbol && ' • '}
            {specificSymbol && <>SS: {specificSymbol}</>}
          </span>
        )}
      </div>
      <span className="font-medium">
        {moneyFormatter.format({ amount: amount ?? '0', currency })}
      </span>
    </div>
  );
}
