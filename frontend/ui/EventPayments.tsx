import { memo } from 'react';
import { EventPaymentsDocument } from '@/graphql/Event';
import { useActionMap } from '@/lib/actions';
import { paymentActions } from '@/lib/actions/payment';
import { ActionRow } from '@/ui/ActionRow';
import { Spinner } from '@/ui/Spinner';
import { moneyFormatter, paymentStatusLabels } from '@/ui/format';
import { useQuery } from 'urql';
import Link from 'next/link';

export const EventPayments = memo(function EventPayments({ id }: { id: string }) {
  const [{ data, fetching, error }] = useQuery({
    query: EventPaymentsDocument,
    variables: { id },
  });

  const event = data?.event;
  const payments = event
    ? [event, ...event.childEventsList].flatMap((e) => e.paymentsList)
    : [];
  const actionMap = useActionMap(paymentActions, payments);

  return fetching && !data ? (
    <div className="flex justify-center py-8">
      <Spinner />
    </div>
  ) : error && !data ? (
    <p>Nepodařilo se načíst platby.</p>
  ) : (
    <div className="divide-y divide-neutral-4">
      {payments.map((payment) => {
        const postings = payment.transactions.nodes.flatMap((t) => t.postingsList);
        return (
          <section key={payment.id} className="py-4 first:pt-0">
            <div className="flex flex-wrap items-center justify-between gap-2">
              <ActionRow actions={actionMap.get(payment.id)!} className="mb-0">
                <Link
                  href={`/platby/${payment.id}`}
                  className="text-accent-11 underline"
                >
                   Platba {payment.id}
                </Link>
              </ActionRow>
              <span className="text-sm text-neutral-11">
                {paymentStatusLabels[payment.status]}
              </span>
            </div>
            {postings.length > 0 ? (
              <div className="mt-2 space-y-1 text-sm">
                {postings.map(({ id, amount, account }) => (
                  <div
                    key={id}
                    className="flex flex-wrap justify-between gap-x-4"
                  >
                    <span>
                      {account?.person?.name ?? (account ? 'Klub' : '–')}
                    </span>
                    <span className="tabular-nums">
                      {moneyFormatter.format({
                        amount,
                        currency: account?.currency ?? 'CZK',
                      })}
                    </span>
                  </div>
                ))}
              </div>
            ) : payment.paymentDebtorsList.length > 0 ? (
              <div className="mt-2 space-y-1 text-sm">
                {payment.paymentDebtorsList.map((debtor) => (
                  <div key={debtor.id} className="flex flex-wrap justify-between gap-x-4">
                    <span>{debtor.person?.name ?? 'Neznámá osoba'}</span>
                    <span className="tabular-nums">
                      {moneyFormatter.format(debtor.price) ?? '–'}
                    </span>
                  </div>
                ))}
              </div>
            ) : null}
          </section>
        );
      })}
      {payments.length === 0 && <p>Žádné platby.</p>}
    </div>
  );
});
