import { Children, Fragment, type ReactNode } from 'react';
import { listFormatter } from '@/ui/format';

export function ListFormat({ children }: { children: ReactNode }) {
  const items = Children.toArray(children);
  if (items.length === 0) return null;

  return listFormatter
    .formatToParts(items.map((_, i) => String(i)))
    .map((x, i) => (
      <Fragment key={i}>
        {x.type === 'element' ? items[Number(x.value)] : x.value}
      </Fragment>
    ));
}
