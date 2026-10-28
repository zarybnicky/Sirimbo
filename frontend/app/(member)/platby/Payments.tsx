'use client';

import { DotSeparated } from '@/ui/DotSeparated';

import { PageHeader } from '@/ui/PageHeader';
import React from 'react';
import { parseAsString, useQueryState } from 'nuqs';
import { Tab, TabMenu } from '@/ui/TabMenu';
import { useClient, useQuery } from 'urql';
import {
  TenantManualCreditTransactionsDocument,
  TenantTurnoverPageDocument,
} from '@/graphql/Payment';
import { PersonAccountsDocument, UnpaidPaymentsDocument } from '@/graphql/Person';
import { describePosting, moneyFormatter, numericDateFormatter } from '@/ui/format';
import { buttonCls } from '@/ui/style';
import { exportBalanceSheet } from '@/ui/reports/export-balance-sheet';
import Link from 'next/link';
import { useAuth } from '@/lib/auth';
import { keyIsNonNull } from '@/lib/truthyFilter';
import { useActionMap } from '@/lib/actions';
import { paymentActions } from '@/lib/actions/payment';
import { ActionGroup } from '@/ui/ActionGroup';
import { usePaginatedQuery } from '@/lib/usePaginatedQuery';
import { Pagination } from '@/ui/Pagination';
import { Table } from '@/ui/Table';

const TURNOVER_PAGE_SIZE = 50;
const DEPOSIT_PAGE_SIZE = 50;

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
        <Tab id="turnover" title="Obrat klubu">
          <TenantTurnover />
        </Tab>
        <Tab id="deposits" title="Dobití kreditu">
          <TenantDeposits />
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

const TenantTurnover = React.memo(function TenantTurnover() {
  const auth = useAuth();
  const { data, fetching, error, pagination } = usePaginatedQuery({
    query: TenantTurnoverPageDocument,
    variables: (offset) => ({ offset, postingsLimit: TURNOVER_PAGE_SIZE }),
    getTotal: (data) =>
      Math.max(
        0,
        ...(data.accountsList ?? []).map((account) => account.postings.totalCount),
      ),
    limit: TURNOVER_PAGE_SIZE,
    pageKey: 'turnoverPage',
  });
  const accounts = data?.accountsList ?? [];
  const columns = [
    'Datum',
    'Popis',
    <span key="amount" className="block text-right">
      Částka
    </span>,
  ];

  if (accounts.length === 0) {
    return (
      <Table columns={columns} busy={fetching} error={error} empty="Žádné pohyby.">
        {null}
      </Table>
    );
  }

  return (
    <div className="flex flex-col gap-6">
      {accounts.map((account) => {
        const postings = account.postings.nodes
          .filter(keyIsNonNull('transaction'))
          .toSorted((a, b) =>
            b.transaction.effectiveDate.localeCompare(a.transaction.effectiveDate),
          );

        return (
          <section key={account.id} className="space-y-2">
            <header className="flex flex-wrap items-baseline justify-between gap-2">
              <h3 className="text-lg font-semibold">Účet {account.currency}</h3>
              <span className="text-sm text-neutral-10">
                Zůstatek:{' '}
                {moneyFormatter.format({
                  amount: account.balance ?? '0',
                  currency: account.currency,
                })}
              </span>
            </header>

            <Table
              columns={columns}
              busy={fetching}
              error={error}
              empty="Žádné zaúčtované pohyby."
            >
              {postings.map((posting) => {
                const transaction = posting.transaction;
                const payment = transaction.payment;
                const variableSymbol = payment?.variableSymbol;
                const specificSymbol = payment?.specificSymbol;

                return (
                  <tr key={posting.id} className="border-t border-neutral-4 align-top">
                    <td className="whitespace-nowrap text-neutral-11">
                      {transaction.effectiveDate
                        ? numericDateFormatter.format(new Date(transaction.effectiveDate))
                        : ''}
                    </td>
                    <td>
                      <div className="flex min-w-48 flex-col gap-1">
                        <span>
                          {transaction.description || describePosting(payment, posting)}
                        </span>
                        {(variableSymbol || specificSymbol) && (
                          <DotSeparated className="text-xs text-neutral-10">
                            {variableSymbol && <>VS: {variableSymbol}</>}
                            {specificSymbol && <>SS: {specificSymbol}</>}
                          </DotSeparated>
                        )}
                        {auth.isAdmin && payment?.id && (
                          <Link
                            href={`/platby/${payment.id}`}
                            className="text-xs font-medium text-accent-11 hover:underline"
                          >
                            Detail platby
                          </Link>
                        )}
                      </div>
                    </td>
                    <td className="whitespace-nowrap text-right font-medium">
                      {moneyFormatter.format({
                        amount: posting.amount ?? '0',
                        currency: account.currency,
                      })}
                    </td>
                  </tr>
                );
              })}
            </Table>
          </section>
        );
      })}
      <Pagination {...pagination} />
    </div>
  );
});

const TenantDeposits = React.memo(function TenantDeposits() {
  const { data, fetching, error, pagination } = usePaginatedQuery({
    query: TenantManualCreditTransactionsDocument,
    variables: (offset) => ({ first: DEPOSIT_PAGE_SIZE, offset }),
    getTotal: (data) => data.transactions?.totalCount,
    limit: DEPOSIT_PAGE_SIZE,
    pageKey: 'depositsPage',
  });

  return (
    <>
      <Table
        columns={[
          'Datum',
          'Osoba',
          'Popis',
          <span key="amount" className="block text-right">
            Částka
          </span>,
        ]}
        busy={fetching}
        error={error}
        empty="Žádná ruční dobití."
      >
        {(data?.transactions?.nodes ?? []).map((transaction) => {
          const personPosting = transaction.postingsList.find(
            (posting) => posting.account?.person,
          );
          const accountPosting = personPosting ?? transaction.postingsList[0];
          const amount = accountPosting?.amount ?? '0';
          const currency = accountPosting?.account?.currency ?? 'CZK';
          const variableSymbol = transaction.payment?.variableSymbol;
          const specificSymbol = transaction.payment?.specificSymbol;

          return (
            <tr key={transaction.id} className="border-t border-neutral-4 align-top">
              <td className="whitespace-nowrap text-neutral-11">
                {transaction.effectiveDate
                  ? numericDateFormatter.format(new Date(transaction.effectiveDate))
                  : ''}
              </td>
              <th scope="row">
                <span className="font-medium">
                  {personPosting?.account?.person?.name ?? '—'}
                </span>
              </th>
              <td>
                <div className="flex min-w-48 flex-col gap-1">
                  <span className="text-neutral-11">
                    {transaction.description || 'Dobití kreditu'}
                  </span>
                  {(variableSymbol || specificSymbol) && (
                    <DotSeparated className="text-xs text-neutral-10">
                      {variableSymbol && <>VS: {variableSymbol}</>}
                      {specificSymbol && <>SS: {specificSymbol}</>}
                    </DotSeparated>
                  )}
                  {transaction.payment?.id && (
                    <Link
                      href={`/platby/${transaction.payment.id}`}
                      className="text-xs font-medium text-accent-11 hover:underline"
                    >
                      Detail platby
                    </Link>
                  )}
                </div>
              </td>
              <td className="whitespace-nowrap text-right font-medium">
                {moneyFormatter.format({ amount, currency })}
              </td>
            </tr>
          );
        })}
      </Table>
      <Pagination {...pagination} />
    </>
  );
});
