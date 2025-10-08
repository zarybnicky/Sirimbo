import { Layout } from '@/components/layout/Layout';
import { PaymentDocument } from '@/graphql/Payment';
import { fullDateFormatter, moneyFormatter, numericDateFormatter } from '@/ui/format';
import { TitleBar } from '@/ui/TitleBar';
import { useTypedRouter, zRouterId } from '@/ui/useTypedRouter';
import React from 'react';
import { useQuery } from 'urql';
import { z } from 'zod';
import Link from 'next/link';
import type { LinkProps } from 'next/link';
import { useAuth } from '@/ui/use-auth';

const QueryParams = z.object({
  id: zRouterId,
});

export default function PaymentPage() {
  const router = useTypedRouter(QueryParams);
  const { id } = router.query;
  const [{ data }] = useQuery({ query: PaymentDocument, variables: { id } });
  const { payment } = data || {};
  const auth = useAuth();
  const debugHref = React.useMemo(() => (payment ? ({ pathname: '/payment/[id]', query: { id: payment.id } } as unknown as LinkProps['href']) : null), [payment]);
  if (!payment) return null;

  return (
    <Layout requireAdmin>
      <TitleBar title="Platby">
        {auth.isAdmin && debugHref && (
          <Link
            href={debugHref}
            className="text-sm font-medium text-accent-11 hover:underline"
          >
            Debug detail
          </Link>
        )}
      </TitleBar>

      <div className="prose prose-accent">
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
                  {moneyFormatter.format({ amount: posting.amount, currency: 'CZK' })}
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
