import { EventPaymentsDocument } from '@/graphql/Event';
import { useActionMap } from '@/lib/actions';
import { paymentActions } from '@/lib/actions/payment';
import { ActionRow } from '@/ui/ActionRow';
import { Spinner } from '@/ui/Spinner';
import { fullDateFormatter, moneyFormatter } from '@/ui/format';
import { useQuery } from 'urql';

export function EventPayments({ id }: { id: string }) {
  const [{ data, fetching }] = useQuery({
    query: EventPaymentsDocument,
    variables: { id },
  });

  const events = data?.eventInstance
    ? [data.eventInstance, ...data.eventInstance.childEventInstancesList]
    : [];
  const payments = events.flatMap((event) =>
    event.paymentsList.flatMap((payment) =>
      payment.transactions.nodes.map((t) => [event, payment, t] as const),
    ),
  );
  const actionMap = useActionMap(
    paymentActions,
    events.flatMap((x) => x.paymentsList),
  );

  return fetching && !data ? (
    <div className="flex justify-center py-8">
      <Spinner />
    </div>
  ) : (
    <div className="prose prose-accent">
      {payments.map(([event, payment, transaction]) => (
        <div key={transaction.id}>
          <ActionRow actions={actionMap.get(payment.id)!} className="mb-0">
            Za lekci {fullDateFormatter.format(new Date(event.since))}
          </ActionRow>
          <ul>
            {transaction.postingsList.map(({ id, amount, account }) => (
              <li key={id}>
                {moneyFormatter.format({ amount: amount, currency: 'CZK' })}
                {' - '}
                {account?.person?.name || (account?.tenantId ? 'Klub' : '-')}
              </li>
            ))}
          </ul>
        </div>
      ))}
    </div>
  );
}
