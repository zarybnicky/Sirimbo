import classNames from "classnames";
import Link from "next/link";

export function List({ children }: {
  children: React.ReactNode;
}) {
  return (
    <div className="flex flex-col px-2 lg:bg-stone-100 lg:dark:bg-gray-900 max-h-screen min-h-screen flex-none w-full lg:w-80 xl:w-96">
      {children}
    </div>
  );
}

function ListTitleBar({ title, children }: {
  title: React.ReactNode;
  children?: React.ReactNode;
}) {
  return (
    <div className="p-4 flex items-center justify-between">
      <div className="font-bold">{title}</div>
      {children}
    </div>
  );
}

function ListTitleButton({ active, icon: Icon, children, href }: {
  children: React.ReactNode;
  active?: boolean;
  icon?: React.ElementType<{ className?: string }>;
  href: string;
}) {
  return (
    <Link href={href} passHref>
      <a className={classNames(
        "button font-bold",
        active ? 'tracking-wide bg-red-500 text-white' : 'bg-white hover:bg-stone-50',
      )}>
        {Icon && <Icon className="w-4" />} {children}
      </a>
    </Link>
  );
}

function ListItem({ active, children, href, title, subtitle }: {
  children: React.ReactNode;
  active?: boolean;
  href: string;
  title: string;
  subtitle?: string;
}) {
  return (
    <Link key={href} href={href} passHref>
      <a className={classNames(
        "relative p-1 pl-6 m-2 rounded-xl grid",
        active ? 'font-semibold bg-red-500 text-white shadow-md' : 'hover:bg-stone-200',
      )}>
        <div>{title}</div>
        <div className={classNames("text-sm", active ? '' : '')}>{subtitle}</div>
        {children}
      </a>
    </Link>
  );
}

function ListScroll({ children }: {
  children: React.ReactNode;
}) {
  return (
    <div className={classNames(
      "scrollbar-thin scrollbar-track-transparent scrollbar-thumb-stone-800/20 hover:scrollbar-thumb-stone-800/50",
      "relative h-full overflow-y-auto border-r border-gray-150 dark:border-gray-800 dark:bg-gray-900",
    )}>
      {children}
    </div>
  );
}

List.TitleBar = ListTitleBar;
List.TitleButton = ListTitleButton;
List.Item = ListItem;
List.Scroll = ListScroll;
