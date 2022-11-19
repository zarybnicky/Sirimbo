import classNames from "classnames";
import Link from "next/link";

export function List({ children }: {
  children: React.ReactNode;
}) {
  return (
    <div className={classNames(
      "flex flex-col px-2 max-h-screen min-h-screen flex-none w-full lg:w-80 xl:w-96",
      "border-r dark:border-gray-800 lg:border-stone-150 lg:bg-stone-50 lg:dark:bg-gray-900")}>
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

function ListTitleButton({ active, icon: Icon, children, href, onClick }: {
  children: React.ReactNode;
  active?: boolean;
  icon?: React.ElementType<{ className?: string }>;
  href?: string;
  onClick?: () => void;
}) {
  const cx = classNames(
    "button px-3 py-1.5 text-xs tracking-tight font-bold",
    active ? 'tracking-wide bg-red-500 text-white' : 'bg-white hover:bg-stone-50',
  );
  return href ? (
    <Link href={href} passHref>
      <a className={cx}>
        {Icon && <Icon className="w-4" />} {children}
      </a>
    </Link>
  ) : (
    <button className={cx} onClick={onClick}>
      {Icon && <Icon className="w-4" />} {children}
    </button>
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
        "relative p-1 pl-6 m-2 rounded-lg grid",
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
      "relative h-full overflow-y-auto",
    )}>
      {children}
    </div>
  );
}

List.TitleBar = ListTitleBar;
List.TitleButton = ListTitleButton;
List.Item = ListItem;
List.Scroll = ListScroll;
