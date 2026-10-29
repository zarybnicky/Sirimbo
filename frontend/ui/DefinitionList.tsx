import { Fragment, type ReactNode } from 'react';

type Props = {
  items: Record<string, ReactNode> | readonly (readonly [string, ReactNode])[];
  className?: string;
};

export function DefinitionList({ items, className = 'text-sm tabular' }: Props) {
  return (
    <dl className={className}>
      {(Array.isArray(items) ? items : Object.entries(items)).map(
        ([label, value], index) =>
          value == null || typeof value === 'boolean' || value === '' ? null : (
            <Fragment key={Array.isArray(items) ? index : label}>
              <dt>{label}</dt>
              <dd>{value}</dd>
            </Fragment>
          ),
      )}
    </dl>
  );
}
