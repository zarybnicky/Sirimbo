import { TitleBar } from '@/ui/TitleBar';
import { Layout } from '@/components/layout/Layout';
import React from 'react';
import { StringParam, useQueryParam } from 'use-query-params';
import { TabMenu } from '@/ui/TabMenu';
import { useMutation, useQuery } from 'urql';
import { TenantTurnoverDocument, type TenantTurnoverQuery } from '@/graphql/Payment';
import { PersonAccountsDocument, UnpaidPaymentsDocument } from '@/graphql/Person';
import { describePosting, moneyFormatter, numericDateFormatter } from '@/ui/format';
import { DropdownMenu, DropdownMenuButton, DropdownMenuContent, DropdownMenuTrigger } from '@/ui/dropdown';
import { MarkAsPaidDocument } from '@/graphql/Payment';
import { buttonCls } from '@/ui/style';
import { exportBalanceSheet } from '@/ui/reports/export-balance-sheet';
import { Spinner } from '@/ui/Spinner';

type TenantAccount = NonNullable<TenantTurnoverQuery['accountsList']>[number];
type TenantPosting = TenantAccount['postingsList'][number];
type TenantTransaction = NonNullable<TenantPosting['transaction']>;

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
  const [{ data, fetching }] = useQuery({ query: TenantTurnoverDocument });
  const accounts = data?.accountsList ?? [];

  if (fetching && accounts.length === 0) {
    return (
      <div className="flex justify-center py-6">
        <Spinner />
      </div>
    );
  }

  if (!fetching && accounts.length === 0) {
    return <p>Žádné pohyby.</p>;
  }

  return (
    <div className="flex flex-col gap-6">
      {accounts.map((account) => {
        const postings = [...account.postingsList]
          .filter((posting): posting is TenantPosting & { transaction: TenantTransaction } => Boolean(posting.transaction))
          .sort((a, b) => (b.transaction.effectiveDate || '').localeCompare(a.transaction.effectiveDate || ''));

        return (
          <section key={account.id} className="space-y-2">
            <header className="flex flex-wrap items-baseline justify-between gap-2">
              <h3 className="text-lg font-semibold">Účet {account.currency}</h3>
              <span className="text-sm text-neutral-500">
                Zůstatek: {moneyFormatter.format({ amount: account.balance ?? '0', currency: account.currency })}
              </span>
            </header>

            {postings.length === 0 ? (
              <p className="text-sm text-neutral-500">Žádné zaúčtované pohyby.</p>
            ) : (
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
            )}
          </section>
        );
      })}
    </div>
  );
}
