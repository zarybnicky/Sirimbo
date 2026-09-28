import { memo } from 'react';
import { EventPaymentsDocument } from '@/graphql/Event';
import { useActionMap } from '@/lib/actions';
import { paymentActions } from '@/lib/actions/payment';
import { ActionRow } from '@/ui/ActionRow';
import { Spinner } from '@/ui/Spinner';
import { fullDateFormatter, moneyFormatter } from '@/ui/format';
import { useQuery } from 'urql';

export const EventPayments = memo(function EventPayments({ id }: { id: string }) {
  const [{ data, fetching }] = useQuery({
    query: EventPaymentsDocument,
    variables: { id },
  });

  const events = data?.eventInstance
    ? [data.eventInstance, ...data.eventInstance.childEventInstancesList]
    : [];
  const payments = events.flatMap((event) =>
    event.paymentsList.map((payment) => [event, payment] as const),
  );
  const actionMap = useActionMap(
    paymentActions,
    payments.map(([, payment]) => payment),
  );

  return fetching && !data ? (
    <div className="flex justify-center py-8">
      <Spinner />
    </div>
  ) : (
    <div className="prose prose-accent">
      {payments.map(([event, payment]) => (
        <div key={payment.id}>
          <ActionRow actions={actionMap.get(payment.id)!} className="mb-0">
            Platba {payment.id} · {fullDateFormatter.format(new Date(event.since))}
          </ActionRow>
          {payment.transactions.nodes.map((transaction) => (
            <ul key={transaction.id}>
              {transaction.postingsList.map(({ id, amount, account }) => (
                <li key={id}>
                  {moneyFormatter.format({ amount, currency: account?.currency ?? 'CZK' })}
                  {' - '}
                  {account?.person?.name || (account?.tenantId ? 'Klub' : '-')}
                </li>
              ))}
            </ul>
          ))}
        </div>
      ))}
    </div>
  );
});
