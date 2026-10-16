import { Fragment, type ReactNode } from 'react';

type Props = {
  items: Record<string, ReactNode>;
  className?: string;
};

export function DefinitionList({ items, className = 'text-sm tabular' }: Props) {
  return (
    <dl className={className}>
      {Object.entries(items).map(([label, value]) =>
        value == null || typeof value === 'boolean' || value === '' ? null : (
          <Fragment key={label}>
            <dt>{label}</dt>
            <dd>{value}</dd>
          </Fragment>
        ),
      )}
    </dl>
  );
}
