import { Children, Fragment, type ReactNode } from 'react';
import { listFormatter } from '@/ui/format';

export function ListFormat({ children }: { children: ReactNode }) {
  const items = Children.toArray(children);
  return listFormatter.formatToParts(items.map((_, index) => String(index))).map(
    (part, index) => (
      <Fragment key={index}>
        {part.type === 'element' ? items[Number(part.value)] : part.value}
      </Fragment>
    ),
  );
}
