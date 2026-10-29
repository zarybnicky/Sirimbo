'use client';

import { PaymentDocument } from '@/graphql/Payment';
import { useActions } from '@/lib/actions';
import { paymentActions } from '@/lib/actions/payment';
import { slugify } from '@/lib/slugify';
import { DefinitionList } from '@/ui/DefinitionList';
import {
  formatEventName,
  moneyFormatter,
  numericFullFormatter,
  paymentStatusLabels,
} from '@/ui/format';
import { PageHeader } from '@/ui/PageHeader';
import { Table } from '@/ui/Table';
import Link from 'next/link';
import { useQuery } from 'urql';

export function Payment({ id }: { id: string }) {
  const [{ data, fetching, error }] = useQuery({
    query: PaymentDocument,
    variables: { id },
    pause: !id,
  });

  const payment = data?.payment;
  const cohort = payment?.cohortSubscription?.cohort;
  const actions = useActions(paymentActions, payment);

  return (
    <>
      <PageHeader
        title={`Platba ${id}`}
        breadcrumbs={[{ label: 'Platby', href: '/platby' }]}
        actions={actions}
      />

      {payment && (
        <DefinitionList
          items={{
            ID: payment.id,
            Stav: paymentStatusLabels[payment.status],
            Vytvořena: numericFullFormatter.format(new Date(payment.createdAt)),
            Splatnost:
              payment.dueAt && numericFullFormatter.format(new Date(payment.dueAt)),
            Uhrazena:
              payment.paidAt && numericFullFormatter.format(new Date(payment.paidAt)),
            'Variabilní symbol': payment.variableSymbol,
            'Specifický symbol': payment.specificSymbol,
            'Členské příspěvky': cohort && (
              <Link
                href={`/treninkove-skupiny/${cohort.id}/${slugify(cohort.name)}`}
                className="text-accent-11 underline"
              >
                {cohort.name}
              </Link>
            ),
            Událost: payment.event && (
              <Link
                href={`/termin/${payment.event.id}`}
                className="text-accent-11 underline"
              >
                {numericFullFormatter.format(new Date(payment.event.since))}
                {' · '}
                {formatEventName(payment.event, 'neutral')}
              </Link>
            ),
          }}
        />
      )}

      <Table
        busy={fetching}
        columns={[
          'Plátce',
          <span key="amount" className="block text-right">
            Částka
          </span>,
        ]}
      >
        {payment?.paymentDebtorsList.map((debtor) => (
          <tr key={debtor.id} className="border-t border-neutral-4 align-top">
            <th scope="row">
              {debtor.person ? (
                <Link
                  href={`/clenove/${debtor.person.id}`}
                  className="text-accent-11 underline"
                >
                  {debtor.person.name}
                </Link>
              ) : (
                '—'
              )}
            </th>
            <td className="whitespace-nowrap text-right">
              {moneyFormatter.format(debtor.price) || '—'}
            </td>
          </tr>
        ))}
      </Table>

      <Table
        className="mt-4"
        columns={[
          'Transakce',
          'Popis',
          'Účet',
          <span key="amount" className="block text-right">
            Částka
          </span>,
        ]}
        busy={fetching}
        error={error}
        empty={payment ? 'Žádné transakce.' : 'Platba nebyla nalezena.'}
      >
        {payment?.transactions.nodes.flatMap((transaction) =>
          transaction.postingsList.map(({ id, account, amount }, index) => (
            <tr key={id} className="border-t border-neutral-4 align-top">
              {index === 0 && (
                <>
                  <td
                    rowSpan={transaction.postingsList.length}
                    className="whitespace-nowrap text-neutral-11"
                  >
                    <time dateTime={transaction.effectiveDate}>
                      {numericFullFormatter.format(new Date(transaction.effectiveDate))}
                    </time>
                  </td>
                  <td rowSpan={transaction.postingsList.length}>
                    {transaction.description || '—'}
                  </td>
                </>
              )}
              <th scope="row" className="pl-2!">
                {account?.personId ? (
                  <Link
                    href={`/clenove/${account.personId}`}
                    className="text-accent-11 underline"
                  >
                    {account.person?.name ?? '—'}
                  </Link>
                ) : account ? (
                  'Klub'
                ) : (
                  '—'
                )}
              </th>
              <td className="whitespace-nowrap text-right">
                {account
                  ? moneyFormatter.format({ amount, currency: account.currency })
                  : amount}
              </td>
            </tr>
          )),
        )}
      </Table>
    </>
  );
}
