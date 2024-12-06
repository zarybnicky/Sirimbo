import { Layout } from '@/components/layout/Layout';
import { PaymentDocument } from '@/graphql/Payment';
import { fullDateFormatter, moneyFormatter, numericDateFormatter } from '@/ui/format';
import { TitleBar } from '@/ui/TitleBar';
import { useTypedRouter, zRouterId } from '@/ui/useTypedRouter';
import React from 'react';
import { useQuery } from 'urql';
import { z } from 'zod';

const QueryParams = z.object({
  id: zRouterId,
});

export default function PaymentPage() {
  const router = useTypedRouter(QueryParams);
  const { id } = router.query;
  const [{ data }] = useQuery({ query: PaymentDocument, variables: { id } });
  const { payment } = data || {};
  if (!payment) return null;

  return (
    <Layout requireAdmin>
      <TitleBar title="Platby" />
      <div className="prose">
        Platba
        {payment.transactions.nodes.map((transaction) => (
          <div key={transaction.id} className="ml-2">
            Transakce
            <span>
              {numericDateFormatter.format(new Date(transaction.effectiveDate))}{' '}
              {transaction.description}
            </span>
            {payment.eventInstance && (
              <div className="flex gap-2 items-center">
                Za lekci {fullDateFormatter.format(new Date(payment.eventInstance.since))}
              </div>
            )}
            <ul>
              {transaction.postingsList.map((posting) => (
                <li key={posting.id}>
                  {moneyFormatter.format(posting.amount)}
                  &nbsp;&nbsp;
                  {posting.account?.person?.name || posting.account?.tenant?.name}
                </li>
              ))}
            </ul>
          </div>
        ))}
      </div>
    </Layout>
  );
};
