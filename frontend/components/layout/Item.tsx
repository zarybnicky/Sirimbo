import Link from 'next/link';
import { CornerLeftUp as UpIcon } from 'react-feather';

export function Item({ className, children }: {
  children: React.ReactNode;
  className?: string;
}) {
  return <div className={"py-12 " + className}>
    {children}
  </div>;
}

function ItemTitleBar({ backHref, title, children }: {
  title: string;
  backHref?: string;
  children?: React.ReactNode;
}) {
  return <>
    {backHref && (
      <div className="block lg:hidden mb-4">
        <Link href={backHref} passHref>
          <a className="flex gap-2 text-red-500 items-center text-sm">
            <UpIcon className="inline-block w-4 h-4" /> Zpět na seznam
          </a>
        </Link>
      </div>
    )}
    <div className="flex gap-2 flex-wrap mb-4 grow-0 h-min justify-between">
      <div className="text-2xl font-bold order-2 w-full md:w-auto md:order-none">{title}</div>
      <div>{children}</div>
    </div>
  </>;
}

Item.Titlebar = ItemTitleBar;