export { createPortal as createPortal_ } from "react-dom"

export const formatMessage_ = (full, msg) => `${msg.constructor.name} ${full ? JSON.stringify(msg) : ""}`

export const formatState_ = (state) => JSON.stringify(state)
