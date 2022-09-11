import * as React from 'react'
import { alpha, useTheme } from '@mui/material/styles'
import Menu, { MenuProps } from '@mui/material/Menu'
import MenuItem, { MenuItemProps } from '@mui/material/MenuItem'
import ArrowRight from '@mui/icons-material/ArrowRight'

export interface NestedMenuItemProps extends Omit<MenuItemProps, 'button'> {
  parentMenuOpen: boolean
  component?: React.ElementType
  label?: React.ReactNode
  rightIcon?: React.ReactNode
  ContainerProps?: React.HTMLAttributes<HTMLElement> &
  React.RefAttributes<HTMLElement | null>
  MenuProps?: Omit<MenuProps, 'children'>
  button?: true | undefined
}

export const NestedMenuItem = React.forwardRef<HTMLLIElement | null, NestedMenuItemProps>(function NestedMenuItem(props, ref) {
  const {
    parentMenuOpen,
    component = 'div',
    label,
    rightIcon = <ArrowRight />,
    children,
    className,
    tabIndex: tabIndexProp,
    MenuProps = {},
    ContainerProps: ContainerPropsProp = {},
    ...MenuItemProps
  } = props

  const { ref: containerRefProp, ...ContainerProps } = ContainerPropsProp;

  const theme = useTheme();

  const menuItemRef = React.useRef<HTMLLIElement>(null)
  const containerRef = React.useRef<HTMLDivElement>(null)
  const menuContainerRef = React.useRef<HTMLDivElement>(null)

  React.useImperativeHandle(ref, () => menuItemRef.current!!)
  React.useImperativeHandle(containerRefProp, () => containerRef.current)

  const [isSubMenuOpen, setIsSubMenuOpen] = React.useState(false)

  const handleMouseEnter = (event: React.MouseEvent<HTMLElement>) => {
    setIsSubMenuOpen(true)
    if (ContainerProps?.onMouseEnter) {
      ContainerProps.onMouseEnter(event)
    }
  }
  const handleMouseLeave = (event: React.MouseEvent<HTMLElement>) => {
    setIsSubMenuOpen(false)
    if (ContainerProps?.onMouseLeave) {
      ContainerProps.onMouseLeave(event)
    }
  }

  // Check if any immediate children are active
  const isSubmenuFocused = () => {
    const active = containerRef.current?.ownerDocument?.activeElement
    return Array.from(menuContainerRef.current?.children ?? []).some(x => x === active);
  }

  const handleFocus = (event: React.FocusEvent<HTMLElement>) => {
    if (event.target === containerRef.current) {
      setIsSubMenuOpen(true)
    }
    if (ContainerProps?.onFocus) {
      ContainerProps.onFocus(event)
    }
  }

  const handleKeyDown = (e: React.KeyboardEvent<HTMLDivElement>) => {
    if (e.key === 'Escape') {
      return
    }
    if (isSubmenuFocused()) {
      e.stopPropagation()
    }
    if (e.key === 'ArrowLeft' && isSubmenuFocused()) {
      containerRef.current?.focus()
    }

    const active = containerRef.current?.ownerDocument?.activeElement
    if (e.key === 'ArrowRight' && e.target === containerRef.current && e.target === active) {
      (menuContainerRef.current?.children[0] as HTMLElement)?.focus();
    }
  }

  const open = isSubMenuOpen && parentMenuOpen

  // Root element must have a `tabIndex` attribute for keyboard navigation
  let tabIndex
  if (!props.disabled) {
    tabIndex = tabIndexProp !== undefined ? tabIndexProp : -1
  }

  return (
    <div
      ref={containerRef}
      onFocus={handleFocus}
      tabIndex={tabIndex}
      onMouseEnter={handleMouseEnter}
      onMouseLeave={handleMouseLeave}
      onKeyDown={handleKeyDown}
    >
      <MenuItem
        {...MenuItemProps}
        className={className}
        ref={menuItemRef}
        sx={{
          backgroundColor: open ? theme.palette.action.hover : 'rgba(0,0,0,0)'
        }}
      >
        {label} {rightIcon}
      </MenuItem>
      <Menu
        style={{ pointerEvents: 'none' }}
        anchorEl={menuItemRef.current}
        anchorOrigin={{ vertical: 'top', horizontal: 'right' }}
        transformOrigin={{ vertical: 'top', horizontal: 'left' }}
        open={open}
        autoFocus={false}
        disableAutoFocus
        disableEnforceFocus
        onClose={() => setIsSubMenuOpen(false)}
        sx={{
          '& .MuiMenu-paper': {
            backgroundColor: alpha(theme.palette.common.white, .9),
            borderRadius: 0,
          },
          '& .MuiListItem-button': {
            fontVariant: 'small-caps',
            display: 'flex',
          },
          '& .MuiListItem-button:hover': {
            color: theme.palette.primary.main,
          },
        }}
        {...ContainerProps}
      >
        <div ref={menuContainerRef} style={{ pointerEvents: 'auto' }}>{children}</div>
      </Menu>
    </div>
  )
})
