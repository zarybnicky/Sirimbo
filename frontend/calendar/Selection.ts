import contains from 'dom-helpers/contains'
import closest from 'dom-helpers/closest'
import { TypedEventTarget } from 'typescript-event-target';

export type ClientPoint = {
  x: number;
  y: number;
  clientX: number;
  clientY: number;
}

type BoxSize = {
  left: number;
  right: number;
  top: number;
  bottom: number;
}

export type Bounds = {
  left: number;
  right: number;
  top: number;
  bottom: number;
  x: number;
  y: number;
}

const addEventListener: <K extends keyof WindowEventMap>(type: K, listener: (ev: WindowEventMap[K]) => any, options?: boolean | AddEventListenerOptions) => () => void = (...args) => {
  window.addEventListener(args[0], args[1], args[2]);
  return () => window.removeEventListener(args[0], args[1]);
}

export function isEvent(node: HTMLElement, { clientX, clientY }: ClientPoint) {
  const target = document.elementFromPoint(clientX, clientY)!
  return !!closest(target, '.rbc-event', node)
}

export function pointInColumn(bounds: BoxSize, point: { x: number; y: number; }) {
  const { left, right, top } = bounds
  const { x, y } = point
  return x < right + 10 && x > left && y > top
}

export function getSlotAtX(rowBox: BoxSize, x: number, slots: number) {
  const cellWidth = (rowBox.right - rowBox.left) / slots
  return Math.floor((x - rowBox.left) / cellWidth)
}

export function pointInBox(box: BoxSize, { x, y }: { x:number,y:number }) {
  return y >= box.top && y <= box.bottom && x >= box.left && x <= box.right
}

const isTouchEvent = (e: any): e is TouchEvent => 'touches' in e && !!e.touches.length;
function getEventCoordinates(e: TouchEvent | DragEvent | MouseEvent) {
  const target = isTouchEvent(e) ? e.touches[0]! : e;
  return {
    clientX: target.clientX,
    clientY: target.clientY,
    pageX: target.pageX,
    pageY: target.pageY,
  }
}

const clickTolerance = 10

interface EventMap {
  reset: CustomEvent<void>;
  click: CustomEvent<ClientPoint>;
  select: CustomEvent<Bounds>;
  selectStart: CustomEvent<ClientPoint & { isTouch: boolean }>;
  selecting: CustomEvent<Bounds>;
  dropFromOutside: CustomEvent<ClientPoint>;
  dragOverFromOutside: CustomEvent<ClientPoint>;
}

class Selection extends TypedEventTarget<EventMap> {
  public selecting = false;
  public isDetached = false;
  public selectRect?: Bounds;
  public removeTouchMoveWindowListener?: () => void;
  public removeInitialEventListener?: () => void;
  public removeEndListener?: () => void;
  public onEscListener?: () => void;
  public removeMoveListener?: () => void;
  public removeKeyUpListener?: () => void;
  public removeKeyDownListener?: () => void;
  public removeDropFromOutsideListener?: () => void;
  public removeDragOverFromOutsideListener?: () => void;
  public initialEventData?: ClientPoint & { isTouch: boolean };

  constructor(
    public container: () => HTMLElement | null,
    public options: { validContainers?: string[], shouldSelect: (point: ClientPoint) => boolean } = { shouldSelect: () => true },
  ) {
    super();
    // Fixes an iOS 10 bug where scrolling could not be prevented on the window.
    // https://github.com/metafizzy/flickity/issues/457#issuecomment-254501356
    this.removeTouchMoveWindowListener = addEventListener('touchmove', () => { /* ignore */ })
    this.removeDropFromOutsideListener = addEventListener('drop', this.dropFromOutsideListener.bind(this))
    this.removeDragOverFromOutsideListener = addEventListener('dragover', this.dragOverFromOutsideListener.bind(this))
    this.addInitialEventListener()
  }

  teardown() {
    this.isDetached = true
    this.removeTouchMoveWindowListener?.()
    this.removeInitialEventListener?.()
    this.removeEndListener?.()
    this.onEscListener?.()
    this.removeMoveListener?.()
    this.removeKeyUpListener?.()
    this.removeKeyDownListener?.()
    this.removeDropFromOutsideListener?.()
    this.removeDragOverFromOutsideListener?.()
  }

  isSelected(node: HTMLElement) {
    const box = this.selectRect
    if (!box || !this.selecting) return false
    return objectsCollide(box, node)
  }

  // Adds a listener that will call the handler only after the user has pressed on the screen
  // without moving their finger for 250ms.
  addLongPressListener(handler: (e: TouchEvent|MouseEvent) => void, initialEvent: TouchEvent) {
    let timer: any = null
    let removeTouchMoveListener: undefined | (() => void);
    let removeTouchEndListener: undefined | (() => void);
    const handleTouchStart = (initialEvent: TouchEvent) => {
      timer = setTimeout(() => {
        cleanup()
        handler(initialEvent)
      }, 250)
      removeTouchMoveListener = addEventListener('touchmove', () => cleanup())
      removeTouchEndListener = addEventListener('touchend', () => cleanup())
    }
    const removeTouchStartListener = addEventListener('touchstart', handleTouchStart)
    const cleanup = () => {
      if (timer) {
        clearTimeout(timer)
        timer = null
      }
      removeTouchMoveListener?.()
      removeTouchMoveListener = undefined
      removeTouchEndListener?.()
      removeTouchEndListener = undefined
    }
    if (initialEvent) {
      handleTouchStart(initialEvent)
    }
    return () => {
      cleanup()
      removeTouchStartListener()
    }
  }

  // Listen for mousedown and touchstart events. When one is received, disable the other and setup
  // future event handling based on the type of event.
  addInitialEventListener() {
    const removeMouseDownListener = addEventListener('mousedown', (e) => {
      this.removeInitialEventListener?.()
      this.handleInitialEvent(e)
      this.removeInitialEventListener = addEventListener('mousedown', this.handleInitialEvent.bind(this))
    })
    const removeTouchStartListener = addEventListener('touchstart', (e) => {
      this.removeInitialEventListener?.()
      this.removeInitialEventListener = this.addLongPressListener(this.handleInitialEvent.bind(this), e)
    })
    this.removeInitialEventListener = () => {
      removeMouseDownListener()
      removeTouchStartListener()
    }
  }

  dropFromOutsideListener(e: DragEvent) {
    e.preventDefault()
    const { pageX, pageY, clientX, clientY } = getEventCoordinates(e)
    const detail = {
      x: pageX,
      y: pageY,
      clientX: clientX,
      clientY: clientY,
    };
    this.dispatchTypedEvent('dropFromOutside', new CustomEvent('dropFromOutside', { detail }))
  }

  dragOverFromOutsideListener(e: DragEvent) {
    e.preventDefault()
    const { pageX, pageY, clientX, clientY } = getEventCoordinates(e)
    const detail = {
      x: pageX,
      y: pageY,
      clientX: clientX,
      clientY: clientY,
    }
    this.dispatchTypedEvent('dragOverFromOutside', new CustomEvent('dragOverFromOutside', { detail }))
  }

  handleInitialEvent(e: MouseEvent | TouchEvent) {
    const { clientX, clientY, pageX, pageY } = getEventCoordinates(e)

    const node = this.container()
    if (!node || this.isDetached) {
      return
    }
    if (e.which === 3 || (e as MouseEvent).button === 2 || !contains(node, document.elementFromPoint(clientX, clientY)!)) {
      return
    }
    if (!contains(node, e.target as HTMLElement) && !objectsCollide(node, { top: pageY, left: pageX, bottom: pageY, right: pageX })) {
      return
    }

    this.initialEventData = {
      isTouch: /^touch/.test(e.type),
      x: pageX,
      y: pageY,
      clientX,
      clientY,
    }
    if (this.options.shouldSelect(this.initialEventData) === false) {
      return
    }

    switch (e.type) {
      case 'mousedown':
        this.removeEndListener = addEventListener('mouseup', this.handleTerminatingEvent.bind(this))
        this.onEscListener = addEventListener('keydown', this.handleTerminatingEvent.bind(this))
        this.removeMoveListener = addEventListener('mousemove', this.handleMoveEvent.bind(this))
        break
      case 'touchstart':
        document.querySelectorAll<HTMLElement>('.rbc-time-content').forEach(x => x.style.overflowY = 'hidden');
        document.querySelectorAll<HTMLElement>('.rbc-time-column').forEach(x => x.style.overflowY = 'hidden');
        document.querySelectorAll<HTMLElement>('body').forEach(x => x.style.overflowY = 'hidden');
        this.handleMoveEvent(e);
        this.removeEndListener = addEventListener('touchend', this.handleTerminatingEvent.bind(this))
        this.removeMoveListener = addEventListener('touchmove', this.handleMoveEvent.bind(this))
        break
    }
  }

  // Check whether provided event target element
  // - is contained within a valid container
  isWithinValidContainer(e: MouseEvent | TouchEvent | KeyboardEvent) {
    if (!this.options.validContainers?.length || !e.target) {
      return true
    }
    return this.options.validContainers.some((target) => !!(e.target as HTMLElement)!.closest(target))
  }

  handleTerminatingEvent(e: MouseEvent | TouchEvent | KeyboardEvent) {
    this.selecting = false

    document.querySelectorAll<HTMLElement>('.rbc-time-content').forEach(x => x.style.overflowY = '');
    document.querySelectorAll<HTMLElement>('.rbc-time-column').forEach(x => x.style.overflowY = '');
    document.querySelectorAll<HTMLElement>('body').forEach(x => x.style.overflowY = '');

    this.removeEndListener?.()
    this.removeMoveListener?.()

    if (!this.initialEventData) return

    const node = this.container();
    const inRoot = !node || contains(node, e.target as HTMLElement);

    const { pageX, pageY } = getEventCoordinates(e as MouseEvent)
    const click = this.isClick(pageX, pageY)

    this.initialEventData = undefined

    if ((e as KeyboardEvent).key === 'Escape' || !this.isWithinValidContainer(e)) {
      return setTimeout(() => this.dispatchTypedEvent('reset', new CustomEvent('reset')));
    }

    if (click && inRoot) {
      const { pageX, pageY, clientX, clientY } = getEventCoordinates(e as MouseEvent)
      const detail = {x: pageX, y: pageY, clientX, clientY}
      return setTimeout(() => this.dispatchTypedEvent('click', new CustomEvent('click', { detail })));
    }

    // User drag-clicked in the Selectable area
    if (!click) {
      return setTimeout(() => this.dispatchTypedEvent('select', new CustomEvent('select', { detail: this.selectRect })));
    }
    return setTimeout(() => this.dispatchTypedEvent('reset', new CustomEvent('reset')));
  }

  handleMoveEvent(e: TouchEvent | MouseEvent) {
    if (!this.initialEventData || this.isDetached) {
      return
    }
    if (e.cancelable) {
      e.preventDefault();
    }

    const { x = 0, y = 0 } = this.initialEventData || {}
    const { pageX, pageY } = getEventCoordinates(e)
    const w = Math.abs(x - pageX)
    const h = Math.abs(y - pageY)
    const left = Math.min(pageX, x)
    const top = Math.min(pageY, y);

    // Prevent emitting selectStart event until mouse is moved.
    // in Chrome on Windows, mouseMove event may be fired just after mouseDown event.
    if (this.isClick(pageX, pageY) && !this.selecting && !(w || h)) {
      return
    }

    this.selectRect = {
      top,
      left,
      x: pageX,
      y: pageY,
      right: left + w,
      bottom: top + h,
    }
    const wasSelecting = this.selecting
    this.selecting = true

    if (!wasSelecting) {
      this.dispatchTypedEvent('selectStart', new CustomEvent('selectStart', { detail: this.initialEventData }))
    }
    if (!this.isClick(pageX, pageY)) {
      this.dispatchTypedEvent('selecting', new CustomEvent('selecting', { detail: this.selectRect }))
    }
  }

  isClick(pageX: number, pageY: number) {
    const { x = 0, y = 0, isTouch } = this.initialEventData || {};
    return !isTouch && Math.abs(pageX - x) <= clickTolerance && Math.abs(pageY - y) <= clickTolerance
  }
}

function objectsCollide(nodeA: HTMLElement | BoxSize, nodeB: HTMLElement | BoxSize, tolerance = 0) {
  const {
    top: aTop,
    left: aLeft,
    right: aRight = aLeft,
    bottom: aBottom = aTop,
  } = getBoundsForNode(nodeA)
  const {
    top: bTop,
    left: bLeft,
    right: bRight = bLeft,
    bottom: bBottom = bTop,
  } = getBoundsForNode(nodeB)

  return !(
    // 'a' bottom doesn't touch 'b' top
    (
      aBottom - tolerance < bTop ||
      // 'a' top doesn't touch 'b' bottom
      aTop + tolerance > bBottom ||
      // 'a' right doesn't touch 'b' left
      aRight - tolerance < bLeft ||
      // 'a' left doesn't touch 'b' right
      aLeft + tolerance > bRight
    )
  )
}

const isElement = (x: unknown): x is HTMLElement => !!(x as Element)?.getBoundingClientRect

export function getBoundsForNode(node: HTMLElement | BoxSize): BoxSize {
  if (!isElement(node)) return node as BoxSize

  const rect = node.getBoundingClientRect();
  const left = rect.left + (window.scrollX || document.body.scrollLeft || 0)
  const top = rect.top + (window.scrollY || document.body.scrollTop || 0)

  return {
    top,
    left,
    right: (node.offsetWidth || 0) + left,
    bottom: (node.offsetHeight || 0) + top,
  }
}

export default Selection
