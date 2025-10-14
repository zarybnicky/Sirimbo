import Link from 'next/link';
import type { LinkProps } from 'next/link';
import * as React from 'react';

import { cn } from '@/ui/cn';
import { moneyFormatter, numericDateFormatter } from '@/ui/format';

interface PaymentTransactionRowProps {
  date?: Date | null;
  primaryLabel?: React.ReactNode;
  secondaryLabel?: React.ReactNode;
  variableSymbol?: string | null;
  specificSymbol?: string | null;
  amount?: string | null;
  currency: string;
  paymentId?: string | null;
  showDebugLink?: boolean;
  className?: string;
}

export const PaymentTransactionRow = React.forwardRef<HTMLDivElement, PaymentTransactionRowProps>(
({
  date,
  primaryLabel,
  secondaryLabel,
  variableSymbol,
  specificSymbol,
  amount,
  currency,
  paymentId,
  showDebugLink,
  className,
}, ref) => {
  const hasSymbols = Boolean(variableSymbol || specificSymbol);
  const shouldShowLink = Boolean(showDebugLink && paymentId);
  const formattedDate = date ? numericDateFormatter.format(date) : '';

  return (
    <div
      ref={ref}
      className={cn(
        'flex flex-wrap items-start justify-between gap-3 bg-neutral-1 px-3 py-2 odd:bg-neutral-1 even:bg-neutral-2',
        className,
      )}
    >
      <span className="min-w-20 text-sm text-neutral-11">{formattedDate}</span>
      <div className="flex min-w-48 flex-1 flex-col gap-1">
        {primaryLabel && <span className={secondaryLabel ? 'font-medium' : undefined}>{primaryLabel}</span>}
        {secondaryLabel && <span className="text-sm text-neutral-11">{secondaryLabel}</span>}
        {hasSymbols && (
          <span className="text-xs text-neutral-10">
            {variableSymbol && <>VS: {variableSymbol}</>}
            {variableSymbol && specificSymbol && ' • '}
            {specificSymbol && <>SS: {specificSymbol}</>}
          </span>
        )}
        {shouldShowLink && paymentId && (
          <Link href={paymentDetailHref(paymentId)} className="text-xs font-medium text-accent-11 hover:underline">
            Detail platby
          </Link>
        )}
      </div>
      <span className="font-medium">{moneyFormatter.format({ amount: amount ?? '0', currency })}</span>
    </div>
  );
});

PaymentTransactionRow.displayName = 'PaymentTransactionRow';

function paymentDetailHref(paymentId: string): LinkProps['href'] {
  return { pathname: '/platby/[id]', query: { id: paymentId } } as LinkProps['href'];
}
