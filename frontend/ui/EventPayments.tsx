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

  const instances = data?.eventInstance
    ? [data.eventInstance, ...data.eventInstance.childEventInstancesList]
    : [];
  const payments = instances.flatMap((instance) =>
    instance.paymentsList.flatMap((payment) =>
      payment.transactions.nodes.map((transaction) => [instance, payment, transaction] as const),
    ),
  );
  const actionMap = useActionMap(
    paymentActions,
    instances.flatMap((instance) => instance.paymentsList),
  );

  if (fetching && !data) {
    return (
      <div className="flex justify-center py-8">
        <Spinner />
      </div>
    );
  }

  return (
    <div className="prose prose-accent">
      {payments.map(([instance, payment, transaction]) => (
        <div key={transaction.id}>
          <ActionRow actions={actionMap.get(payment.id)!} className="mb-0">
            Za lekci {fullDateFormatter.format(new Date(instance.since))}
          </ActionRow>
          <ul>
            {transaction.postingsList.map((posting) => (
              <li key={posting.id}>
                {moneyFormatter.format({ amount: posting.amount, currency: 'CZK' })}
                {' - '}
                {posting.account?.person?.name ||
                  (posting.account?.tenantId ? 'Klub' : '-')}
              </li>
            ))}
          </ul>
        </div>
      ))}
    </div>
  );
}
