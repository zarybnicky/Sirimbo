import { Layout } from '@/ui/Layout';
import { PaymentDocument } from '@/graphql/Payment';
import type { PaymentQuery } from '@/graphql/Payment';
import { moneyFormatter, numericDateFormatter } from '@/ui/format';
import { TitleBar } from '@/ui/TitleBar';
import { useTypedRouter, zRouterId } from '@/ui/useTypedRouter';
import Link from 'next/link';
import React from 'react';
import { useQuery } from 'urql';
import { z } from 'zod';

const QueryParams = z.object({
  id: zRouterId,
});

export default function PaymentPage() {
  const router = useTypedRouter(QueryParams);
  const { id } = router.query;
  const [{ data, fetching, error }] = useQuery({
    query: PaymentDocument,
    variables: { id },
    pause: !id,
  });
  const payment = data?.payment ?? null;

  return (
    <Layout requireAdmin>
      <TitleBar title={`Detail platby ${id ?? ''}`}>
        <Link
          href="/platby"
          className="text-sm font-medium text-accent-11 hover:underline"
        >
          Přehled plateb
        </Link>
      </TitleBar>
      <div className="space-y-6">
        {fetching && <p>Načítám…</p>}
        {error && <p className="text-accent-11">Nepodařilo se načíst platbu.</p>}
        {payment ? (
          <PaymentTree payment={payment} />
        ) : (
          !fetching && !error && <p>Platba nebyla nalezena.</p>
        )}
      </div>
    </Layout>
  );
}

type PaymentData = NonNullable<PaymentQuery['payment']>;
type TransactionNode = NonNullable<PaymentData['transactions']['nodes'][number]>;
type PostingNode = NonNullable<TransactionNode['postingsList'][number]>;
type AccountInfo = NonNullable<PostingNode['account']>;

type PaymentTreeProps = {
  payment: PaymentQuery['payment'];
};

export function PaymentTree({ payment }: PaymentTreeProps) {
  if (!payment) return null;

  const hasTransactions = payment.transactions.nodes.some(Boolean);
  const targetAccounts: AccountInfo[] = [];
  const seenAccounts = new Set<string>();
  for (const transaction of payment.transactions.nodes) {
    if (!transaction) continue;
    for (const posting of transaction.postingsList) {
      if (!posting?.account) continue;
      const { account } = posting;
      if (seenAccounts.has(account.id)) continue;
      seenAccounts.add(account.id);
      targetAccounts.push(account);
    }
  }

  return (
    <div className="space-y-6">
      <section className="rounded-lg border border-neutral-6 bg-neutral-1 p-4 shadow-sm">
        <h2 className="text-lg font-semibold text-neutral-12">Platba</h2>
        <dl className="mt-2 grid gap-y-1 text-sm text-neutral-12 md:grid-cols-2">
          <Info label="ID" value={payment.id} />
          <Info label="Stav" value={payment.status} />
          <Info label="Splatnost" value={formatDate(payment.dueAt)} />
          <Info label="Uhrazena" value={formatDate(payment.paidAt)} />
          <Info label="Vytvořeno" value={formatDate(payment.createdAt)} />
          <Info label="Variabilní symbol" value={payment.variableSymbol} />
          <Info label="Specifický symbol" value={payment.specificSymbol} />
          <Info
            label="Automatické přiřazení"
            value={payment.isAutoCreditAllowed ? 'povoleno' : 'zakázáno'}
          />
        </dl>
        <div className="mt-4 space-y-2 text-sm text-neutral-11">
          {payment.cohortSubscription && (
            <div>
              <h3 className="font-medium text-neutral-12">Předplatné skupiny</h3>
              <p className="flex flex-wrap items-center gap-2">
                <span>#{payment.cohortSubscription.id}</span>
                {payment.cohortSubscription.cohort && (
                  <Link
                    href={{
                      pathname: '/treninkove-skupiny/[id]',
                      query: { id: payment.cohortSubscription.cohort.id },
                    }}
                    className="text-sm font-medium text-accent-11 hover:underline"
                  >
                    {payment.cohortSubscription.cohort.name}
                  </Link>
                )}
              </p>
            </div>
          )}
          {payment.eventInstance && (
            <div>
              <h3 className="font-medium text-neutral-12">Termín události</h3>
              <p className="flex flex-wrap items-center gap-2">
                <span>
                  #{payment.eventInstance.id} –{' '}
                  {numericDateFormatter.format(new Date(payment.eventInstance.since))}
                </span>
                {payment.eventInstance.event && (
                  <Link
                    href={{
                      pathname: '/akce/[id]',
                      query: { id: payment.eventInstance.event.id },
                    }}
                    className="text-sm font-medium text-accent-11 hover:underline"
                  >
                    {payment.eventInstance.event.name}
                  </Link>
                )}
              </p>
              {!!payment.eventInstance.trainersList?.length && (
                <ul className="ml-4 list-disc">
                  {payment.eventInstance.trainersList.map((trainer) => (
                    <li key={trainer.id}>{trainer.person?.name}</li>
                  ))}
                </ul>
              )}
            </div>
          )}
          {payment.eventRegistration && (
            <div>
              <h3 className="font-medium text-neutral-12">Registrace na akci</h3>
              <p className="flex flex-wrap items-center gap-2">
                <span>#{payment.eventRegistration.id}</span>
                {payment.eventRegistration.event && (
                  <Link
                    href={{
                      pathname: '/akce/[id]',
                      query: { id: payment.eventRegistration.event.id },
                    }}
                    className="text-sm font-medium text-accent-11 hover:underline"
                  >
                    {payment.eventRegistration.event.name}
                  </Link>
                )}
              </p>
            </div>
          )}
        </div>
      </section>

      <section className="rounded-lg border border-neutral-6 bg-neutral-1 p-4 shadow-sm">
        <h2 className="text-lg font-semibold text-neutral-12">Transakce</h2>
        <div className="mt-4 space-y-4">
          {hasTransactions ? (
            payment.transactions.nodes.map((transaction) => {
              if (!transaction) return null;

              return (
                <article
                  key={transaction.id}
                  className="rounded-md border border-neutral-6 bg-neutral-1 p-3 shadow-sm"
                >
                  <h3 className="font-medium text-neutral-12">
                    #{transaction.id} –{' '}
                    {numericDateFormatter.format(new Date(transaction.effectiveDate))}
                  </h3>
                  <p className="text-sm text-neutral-11">{transaction.description}</p>
                  <div className="mt-3 space-y-2">
                    {transaction.postingsList.map((posting) => {
                      if (!posting) return null;

                      return (
                        <div
                          key={posting.id}
                          className="rounded border border-dashed border-neutral-6 p-2"
                        >
                          <div className="flex flex-wrap justify-between text-sm text-neutral-12">
                            <span className="font-medium">Zápis #{posting.id}</span>
                            <span>
                              {moneyFormatter.format({
                                amount: posting.amount,
                                currency: 'CZK',
                              })}
                            </span>
                          </div>
                          <AccountSummary account={posting.account} className="mt-2" />
                        </div>
                      );
                    })}
                  </div>
                </article>
              );
            })
          ) : (
            <p className="text-sm text-neutral-11">Žádné transakce.</p>
          )}
        </div>
      </section>

      <section className="rounded-lg border border-neutral-6 bg-neutral-1 p-4 shadow-sm">
        <h2 className="text-lg font-semibold text-neutral-12">Cílové účty</h2>
        <div className="mt-3 space-y-2">
          {targetAccounts.length > 0 ? (
            targetAccounts.map((account) => (
              <AccountSummary key={account.id} account={account} />
            ))
          ) : (
            <p className="text-sm text-neutral-11">Žádné cílové účty.</p>
          )}
        </div>
      </section>

      <section className="rounded-lg border border-neutral-6 bg-neutral-1 p-4 shadow-sm">
        <h2 className="text-lg font-semibold text-neutral-12">Surová data</h2>
        <pre className="mt-2 max-h-[40rem] overflow-auto rounded bg-neutral-2 p-4 text-xs text-neutral-12">
          {JSON.stringify(payment, null, 2)}
        </pre>
      </section>
    </div>
  );
}

type InfoProps = {
  label: string;
  value: React.ReactNode;
};

function Info({ label, value }: InfoProps) {
  if (!value) return null;
  return (
    <div>
      <dt className="text-neutral-11">{label}</dt>
      <dd className="font-medium text-neutral-12">{value}</dd>
    </div>
  );
}

type AccountSummaryProps = {
  account?: AccountInfo | null;
  className?: string;
};

function AccountSummary({ account, className }: AccountSummaryProps) {
  const baseClass =
    'rounded border border-neutral-6 bg-neutral-2 p-2 text-xs text-neutral-11';
  const classes = className ? `${baseClass} ${className}` : baseClass;
  return (
    <div className={classes}>
      {account ? (
        <ul className="space-y-1">
          <li>
            <span className="font-semibold text-neutral-12">Účet:</span> {account.id}
          </li>
          <li>
            <span className="font-semibold text-neutral-12">Osoba:</span>{' '}
            {account.person ? (
              <Link
                href={{ pathname: '/clenove/[id]', query: { id: account.person.id } }}
                className="font-medium text-accent-11 hover:underline"
              >
                {account.person.name} (#{account.person.id})
              </Link>
            ) : (
              '—'
            )}
          </li>
          <li>
            <span className="font-semibold text-neutral-12">Tenant:</span>{' '}
            {account.tenant ? `${account.tenant.name} (#${account.tenant.id})` : '—'}
          </li>
        </ul>
      ) : (
        <p className="text-neutral-11">Žádný účet není přiřazen.</p>
      )}
    </div>
  );
}

function formatDate(value: string | null | undefined) {
  if (!value) return null;
  return numericDateFormatter.format(new Date(value));
}
