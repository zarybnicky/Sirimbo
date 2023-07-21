import React from 'react';
import Link from 'next/link';
import * as DropdownMenuPrimitive from '@radix-ui/react-dropdown-menu';
import { cn } from './cn';
import { MoreVertical } from 'lucide-react';

export const DropdownMenu = DropdownMenuPrimitive.Root;

export const DropdownMenuTrigger = DropdownMenuPrimitive.Trigger;

export const DropdownMenuItem = DropdownMenuPrimitive.Item;

export const DropdownMenuPortal = DropdownMenuPrimitive.Portal;

export const DropdownMenuContent = React.forwardRef<
  React.ElementRef<typeof DropdownMenuPrimitive.Content>,
  React.ComponentPropsWithoutRef<typeof DropdownMenuPrimitive.Content>
>(({ className, children, ...props }, ref) => (
  <DropdownMenuPortal>
    <DropdownMenuPrimitive.Content
      ref={ref}
      className={cn(
        'min-w-[220px] bg-white rounded-md p-[5px] z-30 flex flex-col',
        'shadow-[0px_10px_38px_-10px_rgba(22,_23,_24,_0.35),_0px_10px_20px_-15px_rgba(22,_23,_24,_0.2)] will-change-[opacity,transform]',
        'data-[side=top]:animate-slideDownAndFade data-[side=right]:animate-slideLeftAndFade data-[side=bottom]:animate-slideUpAndFade data-[side=left]:animate-slideRightAndFade',
        className,
      )}
      {...props}
    >
      <DropdownMenuPrimitive.Arrow className="fill-current text-white" />
      {children}
    </DropdownMenuPrimitive.Content>
  </DropdownMenuPortal>
));
DropdownMenuContent.displayName = DropdownMenuPrimitive.Content.displayName;

export const DropdownMenuLink = React.forwardRef<
  React.ElementRef<typeof DropdownMenuPrimitive.Item>,
  React.ComponentPropsWithoutRef<typeof Link>
>(({ className, children, ...props }, ref) => (
  <DropdownMenuItem asChild ref={ref}>
  <Link
    className={cn(
      'group text-sm leading-none text-primary bg-neutral-1 rounded-[3px] flex items-center p-3 relative pl-[25px]',
      'select-none outline-none',
      'data-[disabled]:text-neutral-11 data-[disabled]:pointer-events-none cursor-pointer',
      'data-[highlighted]:bg-primary data-[highlighted]:text-white data-[highlighted]:font-bold',
      className,
    )}
    {...props}
  >
    {children}
  </Link>
  </DropdownMenuItem>
));

export const DropdownMenuButton = React.forwardRef<
  HTMLButtonElement,
  React.HTMLAttributes<HTMLButtonElement>
>(({ className, children, ...props }, ref) => (
  <DropdownMenuItem asChild>
  <button
    ref={ref}
    className={cn(
      'group text-sm leading-none text-primary bg-neutral-1 rounded-[3px] flex items-center p-3 relative pl-[25px]',
      'select-none outline-none',
      'data-[disabled]:text-neutral-11 data-[disabled]:pointer-events-none cursor-pointer',
      'data-[highlighted]:bg-primary data-[highlighted]:text-white data-[highlighted]:font-bold',
      className,
    )}
    {...props}
  >
    {children}
  </button>
  </DropdownMenuItem>
));

export const DropdownMenuTriggerDots = React.forwardRef<
  React.ElementRef<typeof DropdownMenuPrimitive.Trigger>,
  React.ComponentPropsWithoutRef<typeof DropdownMenuPrimitive.Trigger>
>(({ className, ...props }, ref) => (
  <DropdownMenuPrimitive.Trigger asChild {...props}>
    <button ref={ref} className={cn("absolute right-1 top-2", className)}>
      <MoreVertical className="text-neutral-7 w-6 group:data-[state=open]:text-neutral-9 group-hover:text-neutral-8" />
    </button>
  </DropdownMenuPrimitive.Trigger>
));
