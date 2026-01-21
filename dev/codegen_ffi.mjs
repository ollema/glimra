import { execFileSync } from 'node:child_process'
import { Error, Ok } from '../glimra/gleam.mjs'

export function run_generate_expected_tokens(configJson) {
  try {
    const output = execFileSync('node', ['dev/generate_expected_tokens.mjs', configJson], {
      encoding: 'utf-8',
      stdio: ['pipe', 'pipe', 'inherit'],
    })
    return new Ok(output)
  }
  catch (error) {
    // If the command ran but failed, include stdout if available
    const message = error.stdout || error.message
    return new Error(message)
  }
}

export function run_extract_patterns(configJson) {
  try {
    const output = execFileSync('node', ['dev/extract_patterns.mjs', configJson], {
      encoding: 'utf-8',
      stdio: ['pipe', 'pipe', 'inherit'],
    })
    return new Ok(output)
  }
  catch (error) {
    const message = error.stdout || error.message
    return new Error(message)
  }
}

export function run_generate_expected_ast(configJson) {
  try {
    const output = execFileSync('node', ['dev/generate_expected_ast.mjs', configJson], {
      encoding: 'utf-8',
      stdio: ['pipe', 'pipe', 'inherit'],
    })
    return new Ok(output)
  }
  catch (error) {
    const message = error.stdout || error.message
    return new Error(message)
  }
}
