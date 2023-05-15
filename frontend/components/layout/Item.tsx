import Link from 'next/link';
import { Route } from 'nextjs-routes';
import { CornerLeftUp as UpIcon } from 'react-feather';
import cx from 'classnames';

type ItemProps = {
    children: React.ReactNode;
    className?: string;
};

export function Item({className, children}: ItemProps) {
  return <div className={cx('px-2 py-4 lg:py-12 lg:px-8', className)}>{children}</div>;
}

type TitleBarProps = {
  title: string;
  backHref?: Route | Exclude<Route, {query: any}>["pathname"];
  children?: React.ReactNode;
};

function ItemTitleBar({backHref, title, children,}: TitleBarProps) {
  return (
    <>
      {backHref && (
        <div className="block lg:hidden mb-4">
          <Link href={backHref} className="flex gap-2 text-red-500 items-center text-sm">
            <UpIcon className="inline-block w-4 h-4" /> Zpět na seznam
          </Link>
        </div>
      )}
      <div className="flex gap-2 flex-wrap mb-4 grow-0 h-min justify-between">
        <h4 className="text-2xl tracking-wide mb-4 order-2 w-full md:w-auto md:order-none">
          {title}
        </h4>
        <div className="flex items-center flex-wrap">{children}</div>
      </div>
    </>
  );
}

Item.Titlebar = ItemTitleBar;
