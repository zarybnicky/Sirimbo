import React from 'react';
import Link from 'next/link';
import * as DropdownMenuPrimitive from '@radix-ui/react-dropdown-menu';
import { cn } from '@/ui/cn';
import { MoreHorizontal, MoreVertical } from 'lucide-react';

export const DropdownMenu = DropdownMenuPrimitive.Root;

export const DropdownMenuTrigger = Object.assign(
  DropdownMenuPrimitive.Trigger,
  {
    CornerDots({ className, ...props }: React.ComponentPropsWithoutRef<typeof DropdownMenuPrimitive.Trigger>) {
      return (
        <DropdownMenuPrimitive.Trigger {...props} className={cn("absolute right-1 top-2", className)}>
          <MoreVertical className="text-neutral-10 w-6 group:data-[state=open]:text-neutral-12 group-hover:text-neutral-11" />
        </DropdownMenuPrimitive.Trigger>
      );
    },
    RowDots(props: React.ComponentPropsWithoutRef<typeof DropdownMenuPrimitive.Trigger>) {
      return (
        <DropdownMenuPrimitive.Trigger {...props}>
          <MoreHorizontal className="size-5 text-neutral-10" />
        </DropdownMenuPrimitive.Trigger>
      );
    },
  }
);

export const DropdownMenuLabel = React.forwardRef<
  React.ElementRef<typeof DropdownMenuPrimitive.Label>,
  React.ComponentPropsWithoutRef<typeof DropdownMenuPrimitive.Label>
>(({ className, children, ...props }, ref) => (
  <DropdownMenuPrimitive.Label
    ref={ref}
    className={cn("text-neutral-11 text-sm bg-neutral-1 pl-3 pt-2", className)}
    {...props}
  >
    {children}
  </DropdownMenuPrimitive.Label>
));
DropdownMenuLabel.displayName = DropdownMenuPrimitive.Label.displayName;

type DropdownMenuContentProps = React.ComponentPropsWithoutRef<typeof DropdownMenuPrimitive.Content>;
const DropdownMenuContentWrapper = React.forwardRef<
  React.ElementRef<typeof DropdownMenuPrimitive.Content>,
  DropdownMenuContentProps
>(({ className, children, ...props }, ref) => (
  <DropdownMenuPrimitive.Portal>
    <DropdownMenuPrimitive.Content
      ref={ref}
      className={cn(
        'min-w-[220px] bg-neutral-2 rounded-md p-[5px] z-30 flex flex-col',
        'shadow-[0px_10px_38px_-10px_rgba(22,_23,_24,_0.35),_0px_10px_20px_-15px_rgba(22,_23,_24,_0.2)] will-change-[opacity,transform]',
        'data-[side=top]:animate-slideDownAndFade data-[side=right]:animate-slideLeftAndFade data-[side=bottom]:animate-slideUpAndFade data-[side=left]:animate-slideRightAndFade',
        "max-h-[calc(var(--radix-dropdown-menu-content-available-height)-var(--radix-dropdown-menu-trigger-height)-15px)]",
        "overflow-y-auto",
        className,
      )}
      {...props}
    >
      <DropdownMenuPrimitive.Arrow className="fill-current text-neutral-0" />
      {children}
    </DropdownMenuPrimitive.Content>
  </DropdownMenuPrimitive.Portal>
));
DropdownMenuContentWrapper.displayName = DropdownMenuPrimitive.Content.displayName;

export const DropdownMenuContent = Object.assign(
  DropdownMenuContentWrapper,
  {
    Actions({ actions, ...props }: { actions: React.ReactNode[]; } & DropdownMenuContentProps) {
      return (
        <DropdownMenuContentWrapper {...props}>
          {actions}
        </DropdownMenuContentWrapper>
      );
    }
  }
);

export const DropdownMenuLink = React.forwardRef<
  React.ElementRef<typeof DropdownMenuPrimitive.Item>,
  React.ComponentPropsWithoutRef<typeof Link>
>(({ className, children, ...props }, ref) => (
  <DropdownMenuPrimitive.Item asChild ref={ref}>
  <Link
    className={cn(
      'group text-sm leading-none text-accent-11 bg-neutral-1 rounded-[3px] inline-flex gap-3 items-center p-3 relative pl-4',
      'select-none outline-none',
      'data-[disabled]:text-neutral-11 data-[disabled]:pointer-events-none cursor-pointer',
      'data-[highlighted]:bg-primary data-[highlighted]:text-white data-[highlighted]:font-bold',
      className,
    )}
    {...props}
  >
    {children}
  </Link>
  </DropdownMenuPrimitive.Item>
));
DropdownMenuLink.displayName = DropdownMenuPrimitive.Item.displayName;

export const DropdownMenuButton = React.forwardRef<
  React.ElementRef<typeof DropdownMenuPrimitive.Item>,
  React.ComponentPropsWithoutRef<typeof DropdownMenuPrimitive.Item>
>(({ className, children, ...props }, ref) => (
  <DropdownMenuPrimitive.Item
    ref={ref}
    className={cn(
      'group text-sm leading-none text-accent-11 bg-neutral-1 rounded-[3px] inline-flex gap-3 items-center p-3 relative pl-4',
      'select-none outline-none',
      'data-[disabled]:text-neutral-11 data-[disabled]:pointer-events-none cursor-pointer',
      'data-[highlighted]:bg-primary data-[highlighted]:text-white data-[highlighted]:font-bold',
      className,
    )}
    {...props}
  >
    {children}
  </DropdownMenuPrimitive.Item>
));
DropdownMenuButton.displayName = DropdownMenuPrimitive.Trigger.displayName;
