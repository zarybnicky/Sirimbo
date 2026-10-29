'use client';

import React from 'react';
import Link from 'next/link';
import type { PaymentWithDebtorsFragment } from '@/graphql/Payment';
import { useActionMap } from '@/lib/actions';
import { paymentActions } from '@/lib/actions/payment';
import { ActionGroup } from '@/ui/ActionGroup';
import {
  describePosting,
  moneyFormatter,
  numericDateWithYearFormatter,
  paymentStatusLabels,
} from '@/ui/format';
import { Table } from '@/ui/Table';

export function PaymentTable({
  payments,
  busy,
  error,
}: {
  payments: readonly PaymentWithDebtorsFragment[];
  busy?: boolean;
  error?: Error | null;
}) {
  const actionMap = useActionMap(paymentActions, payments);
  return (
    <Table
      columns={[
        <span key="actions" className="sr-only">
          Akce
        </span>,
        'Platba',
        'Stav',
        'Splatnost',
        'Osoba',
        <span key="amount" className="block text-right">
          Částka
        </span>,
      ]}
      busy={busy}
      error={error}
      empty="Žádné platby."
    >
      {payments.map((payment) => {
        const [debtor, ...others] = payment.paymentDebtorsList;
        const rowSpan = others.length + 1;
        return (
          <React.Fragment key={payment.id}>
            <tr className="border-t border-neutral-4 align-top">
              <td rowSpan={rowSpan} className="w-0">
                <ActionGroup variant="row" actions={actionMap.get(payment.id)!} />
              </td>
              <th scope="row" rowSpan={rowSpan}>
                <Link href={`/platby/${payment.id}`} className="text-accent-11 underline">
                  {describePosting(payment, 'neutral') || `Platba ${payment.id}`}
                </Link>
                {(payment.variableSymbol || payment.specificSymbol) && (
                  <div className="flex flex-wrap gap-x-2 text-xs text-neutral-11">
                    {payment.variableSymbol && <span>VS: {payment.variableSymbol}</span>}
                    {payment.specificSymbol && <span>SS: {payment.specificSymbol}</span>}
                  </div>
                )}
              </th>
              <td rowSpan={rowSpan} className="text-neutral-11">
                {paymentStatusLabels[payment.status]}
              </td>
              <td rowSpan={rowSpan} className="whitespace-nowrap text-neutral-11">
                {payment.dueAt
                  ? numericDateWithYearFormatter.format(new Date(payment.dueAt))
                  : ''}
              </td>
              <td>{debtor?.person?.name}</td>
              <td className="whitespace-nowrap text-right">
                {moneyFormatter.format(debtor?.price ?? null)}
              </td>
            </tr>
            {others.map((debtor) => (
              <tr key={debtor.id} className="border-t border-neutral-4 align-top">
                <td className="pl-2!">{debtor.person?.name}</td>
                <td className="whitespace-nowrap text-right">
                  {moneyFormatter.format(debtor.price)}
                </td>
              </tr>
            ))}
          </React.Fragment>
        );
      })}
    </Table>
  );
}
