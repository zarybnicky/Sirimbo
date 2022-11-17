import Link from 'next/link';
import { CornerLeftUp as UpIcon } from 'react-feather';

export function Item() { }

function ItemTitleBar({ backHref, title, children }: {
  title?: string;
  backHref: string;
  children: React.ReactNode;
}) {
  return <>
    <div>
      <div className="block lg:hidden mb-4">
        <Link href={backHref} passHref>
          <a className="flex gap-2 text-red-500 items-center text-sm">
            <UpIcon className="inline-block w-4 h-4" /> Zpět na seznam
          </a>
        </Link>
      </div>
    </div>
    <div className="flex grow-0 h-min justify-between">
      <div className="text-xl font-bold">{title}</div>
      <div>
        {children}
      </div>
    </div>
  </>;
}

Item.Titlebar = ItemTitleBar;
